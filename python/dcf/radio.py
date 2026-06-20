# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF-Radio — turn live DCF-Audio into a digital-radio stream with DVR rewind.

A *station* is one frequency channel (the frame ``dst``); every peer talking on
that channel is mixed into one live program. Each station runs a per-station
**writer thread** that paces a continuous frame stream into a persistent
``dcf`` -> ffmpeg HLS encoder — real audio when it arrives, **codec-correct silence
when it doesn't**, so the live edge never freezes (no dead air) and the DVR window
has no holes. Captured *real* frames are also appended to a rotating
``radio-CH<dst>.dcf`` archive — the compact, exact past, re-renderable with
``dcf-rec rec``. A stdlib HTTP server publishes playlists, a player, and
``/healthz`` / ``/metrics`` / ``/stations.json`` (now-playing).

Ingress (both feed :meth:`RadioServer.feed`): the wiretap (``dcf-rec stream``) and
an AUTO/master node (``dcf_node.py start --radio``). Reuses the certified frame
``decode`` + ``packetize`` and the native ``dcf`` ffmpeg demuxer; no wire/cert
changes. The wire is plaintext — keep HTTP on localhost/trusted nets and run the
UDP side under WireGuard (DCF_SECURITY_EXPOSURE.md).
"""
import collections
import json
import os
import re
import subprocess
import sys
import threading
import time
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer

_HERE = os.path.dirname(os.path.abspath(__file__))
_ROOT = os.path.dirname(_HERE)
sys.path.insert(0, os.path.join(_ROOT, "python", "MCP"))
from wirelab_core import decode                       # certified DeModFrame decode
import audiolab_core as al                            # packetize + codec ids + reasm

FRAME_SIZE = 17
HLS_SEG_SECONDS = 2
DEFAULT_DVR = 6 * 3600                                # 6 h
TICK = 0.020                                          # 20 ms writer cadence
GAP_FILL_AFTER = 0.045                                # start silence after a 45 ms gap
MIX_REAP_S = 10.0                                     # drop a silent peer from the amix
STATION_REAP_S = 300.0                                # stop the encoder after 5 min dead air
MAX_PENDING = 500                                     # ~10 s of buffered blocks per peer
START_EPOCH = time.time()

# Codec-correct 20 ms silence payloads (fed when a peer is quiet, to avoid dead air).
SILENCE = {
    al.CODEC_OPUS:     bytes([0xF8, 0xFF, 0xFE]),     # mono 20 ms Opus silence TOC
    al.CODEC_PCM_DIAG: bytes([0x80]) * al.PCM_DIAG_BLOCK,  # 120 samples at mid = silent
    al.CODEC_FAUST_PM: bytes(8),                      # amp=0 PM params = silent
}


def ffmpeg_bin():
    return os.environ.get("FFMPEG", "ffmpeg")


def parse_duration(s):
    """'6h' / '30m' / '90s' / '120' -> seconds (int)."""
    if s is None:
        return DEFAULT_DVR
    m = re.fullmatch(r"\s*(\d+)\s*([hms]?)\s*", str(s).lower())
    if not m:
        raise ValueError(f"bad duration {s!r} (use e.g. 6h, 30m, 90s)")
    return int(m.group(1)) * {"h": 3600, "m": 60, "s": 1, "": 1}[m.group(2)]


def parse_names(spec):
    """'0x00a1=Hermes,66=Bob' -> {0x00a1: 'Hermes', 66: 'Bob'} (mirrors mesh_viz)."""
    out = {}
    for part in (spec or "").split(","):
        part = part.strip()
        if "=" in part:
            k, v = part.split("=", 1)
            try:
                out[int(k.strip(), 0)] = v.strip()
            except ValueError:
                pass
    return out


def channel_label(dst):
    return f"CH{dst}"


class _Opts:
    """Encoder/station options (shared across a server's stations)."""
    def __init__(self, dvr_s=DEFAULT_DVR, archive_dir=None, archive_max_mb=512,
                 codec="aac", bitrate="96k", loudnorm=False, live_mp3=True, names=None):
        self.dvr_s = dvr_s
        self.archive_dir = archive_dir
        self.archive_max = archive_max_mb * 1024 * 1024
        self.codec = codec
        self.bitrate = bitrate
        self.loudnorm = loudnorm
        self.live_mp3 = live_mp3
        self.names = names or {}


class RadioStation:
    """One channel's continuous HLS program (+ DVR window + rotating .dcf archive)."""

    def __init__(self, dst, hls_dir, opts, log=print, start_thread=True):
        self.dst = dst
        self.label = channel_label(dst)
        self.hls_dir = hls_dir
        self.opts = opts
        self.log = log
        self.m3u8 = os.path.join(hls_dir, self.label + ".m3u8")
        self.seg_glob = self.label + "_%05d.ts"
        self.started = time.time()
        # counters (observability)
        self.frames_real = 0
        self.frames_silence = 0
        self.dropped = 0
        # per-source playout state, guarded by _lock
        self._srcs = {}                  # src -> dict(reasm,pending,played,last_real,codec,flags)
        self._lock = threading.Lock()
        # encoder (touched only by the writer thread — no lock needed)
        self._proc = None
        self._mix = []                   # ordered src ids the encoder was built for
        self._last_relaunch = 0.0
        self._live = None                # optional Popen for the /live.mp3 endpoint
        os.makedirs(hls_dir, exist_ok=True)
        self._archive = None
        self._archive_path = None
        if opts.archive_dir:
            os.makedirs(opts.archive_dir, exist_ok=True)
            self._archive_path = os.path.join(opts.archive_dir, f"radio-{self.label}.dcf")
            self._archive = open(self._archive_path, "ab")
        # writer thread (gated so tests can drive _tick() deterministically)
        self._stop = threading.Event()
        self._thread = threading.Thread(target=self._writer_loop, name=f"radio-{self.label}",
                                        daemon=True)
        if start_thread:
            self._thread.start()

    # ── ingest (called from the capture thread — never blocks on ffmpeg) ───────
    def feed(self, frame, src):
        with self._lock:
            if self._archive:
                self._archive.write(frame)          # archive: compact, real audio only
                self._maybe_rotate()
            s = self._srcs.get(src)
            if s is None:
                s = {"reasm": al.AudioReassembler(), "pending": collections.deque(),
                     "played": 0, "last_real": time.time(), "codec": al.CODEC_OPUS,
                     "flags": 0}
                self._srcs[src] = s
            for ev in s["reasm"].push(frame):
                if ev[0] == "packet":
                    _, _pid, _ts, codec_id, payload, flags = ev
                    s["codec"] = codec_id
                    s["flags"] = flags
                    s["pending"].append((codec_id, payload, flags))
                    if len(s["pending"]) > MAX_PENDING:
                        s["pending"].popleft()
                        self.dropped += 1

    # ── writer: paced real+silence frame stream -> ffmpeg (no dead air) ────────
    def _writer_loop(self):
        next_t = time.monotonic()
        while not self._stop.is_set():
            try:
                self._tick()
            except Exception as e:                  # never let the writer die
                self.log(f"radio {self.label}: writer error {e}")
            next_t += TICK
            self._stop.wait(max(0.0, next_t - time.monotonic()))

    def _tick(self):
        now = time.time()
        out = bytearray()
        with self._lock:
            mix_now = []
            for src, s in self._srcs.items():
                wrote_real = False
                while s["pending"]:
                    cid, payload, fl = s["pending"].popleft()
                    out += self._block(src, s, cid, payload, fl)
                    s["last_real"] = now
                    self.frames_real += 1
                    wrote_real = True
                idle = now - s["last_real"]
                if not wrote_real and GAP_FILL_AFTER < idle < STATION_REAP_S:
                    # quiet but live -> one silence block keeps the edge advancing
                    out += self._block(src, s, s["codec"], SILENCE.get(s["codec"], b""), 0)
                    self.frames_silence += 1
                if idle < MIX_REAP_S:
                    mix_now.append(src)
            mix_now.sort()
        # ffmpeg I/O happens OUTSIDE the lock so ingest never stalls.
        self._ensure_encoder(mix_now)
        if out and self._proc and self._proc.stdin:
            try:
                self._proc.stdin.write(out)
                self._proc.stdin.flush()
                if self._live and self._live.stdin:
                    self._live.stdin.write(out)
                    self._live.stdin.flush()
            except (BrokenPipeError, ValueError, OSError):
                self._proc = None                   # respawned next tick

    def _block(self, src, s, codec_id, payload, flags):
        """Packetize one 20 ms block at the monotonic playout pid; bump played."""
        pid = s["played"] & al.MAX_PACKET_ID
        ts = (s["played"] * 20000) & 0xFFFFFF
        s["played"] += 1
        return b"".join(al.packetize(codec_id, payload, pid, ts, src, self.dst, flags))

    # ── encoder (writer-thread only) ───────────────────────────────────────────
    def _next_seg_number(self):
        hi, pat = -1, re.compile(re.escape(self.label) + r"_(\d+)\.ts$")
        try:
            for name in os.listdir(self.hls_dir):
                m = pat.match(name)
                if m:
                    hi = max(hi, int(m.group(1)))
        except OSError:
            pass
        return hi + 1

    def _filtergraph(self, n):
        chain = "aresample=async=1:first_pts=0"
        if self.opts.loudnorm:
            chain += ",loudnorm=I=-16:TP=-1.5:LRA=11"
        if n == 1:
            return ["-af", chain]
        ins = "".join(f"[0:a:{i}]" for i in range(n))
        return ["-filter_complex", f"{ins}amix=inputs={n}:normalize=0,{chain}[a]",
                "-map", "[a]"]

    def _spawn(self, mix):
        n = max(1, len(mix))
        list_size = max(3, self.opts.dvr_s // HLS_SEG_SECONDS)
        seg_type = "fmp4" if self.opts.codec == "opus" else "mpegts"
        acodec = "libopus" if self.opts.codec == "opus" else "aac"
        cmd = [ffmpeg_bin(), "-hide_banner", "-loglevel", "error", "-f", "dcf", "-i", "pipe:0"]
        cmd += self._filtergraph(n)
        cmd += ["-c:a", acodec, "-b:a", self.opts.bitrate, "-ar", "48000", "-ac", "1",
                "-f", "hls", "-hls_time", str(HLS_SEG_SECONDS),
                "-hls_list_size", str(list_size),
                "-start_number", str(self._next_seg_number()),
                "-hls_segment_type", seg_type,
                "-hls_flags", "delete_segments+append_list+program_date_time+omit_endlist",
                "-hls_segment_filename", os.path.join(self.hls_dir, self.seg_glob),
                self.m3u8]
        self._proc = subprocess.Popen(cmd, stdin=subprocess.PIPE,
                                      stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        self._mix = list(mix)
        self._last_relaunch = time.time()
        self.log(f"radio {self.label}: encoder up (mix={n} {self.opts.codec})")
        if self.opts.live_mp3:
            self._spawn_live()

    def _spawn_live(self):
        """A continuous low-latency MP3 of the live edge (Icecast-style, no rewind)."""
        try:
            if self._live and self._live.poll() is None:
                self._live.stdin.close()
        except OSError:
            pass
        path = os.path.join(self.hls_dir, self.label + ".live.mp3")
        # The encoder also tees frames to this proc; it writes a growing MP3 the HTTP
        # server streams from the tail. Single program (map first) for low latency.
        cmd = [ffmpeg_bin(), "-hide_banner", "-loglevel", "error", "-f", "dcf", "-i", "pipe:0",
               "-map", "0:a:0", "-af", "aresample=async=1:first_pts=0",
               "-c:a", "libmp3lame", "-b:a", "96k", "-ar", "48000", "-ac", "1",
               "-f", "mp3", "-y", path]
        self._live = subprocess.Popen(cmd, stdin=subprocess.PIPE,
                                      stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        self._live_path = path

    def _ensure_encoder(self, mix_now):
        dead = self._proc is None or self._proc.poll() is not None
        changed = mix_now != self._mix and (time.time() - self._last_relaunch) > 1.0
        if not mix_now and not dead and not changed:
            return
        if dead:
            self._spawn(mix_now or self._mix or [0])
        elif changed:
            try:
                self._proc.stdin.close()
                self._proc.wait(timeout=3)
            except (OSError, subprocess.TimeoutExpired):
                try:
                    self._proc.kill()
                except OSError:
                    pass
            self._spawn(mix_now)

    # ── archive rotation ───────────────────────────────────────────────────────
    def _maybe_rotate(self):
        if not self._archive or self.opts.archive_max <= 0:
            return
        if self._archive.tell() >= self.opts.archive_max:
            self._archive.close()
            ts = time.strftime("%Y%m%d-%H%M%S")
            os.replace(self._archive_path, f"{self._archive_path}.{ts}")
            self._archive = open(self._archive_path, "ab")

    # ── observability ──────────────────────────────────────────────────────────
    def _peer_name(self, src):
        return self.opts.names.get(src, f"peer 0x{src:04x}")

    def info(self):
        now = time.time()
        with self._lock:
            peers = []
            for src in sorted(self._srcs):
                s = self._srcs[src]
                peers.append({"id": f"0x{src:04x}", "src": src, "name": self._peer_name(src),
                              "talking": (now - s["last_real"]) < 0.3,
                              "blocks": s["played"]})
        return {
            "channel": self.dst, "label": self.label,
            "name": self.opts.names.get(self.dst, self.label),
            "peers": peers, "num_peers": len(peers),
            "frames_real": self.frames_real, "frames_silence": self.frames_silence,
            "dropped": self.dropped, "uptime_s": round(now - self.started, 1),
            "playlist": f"/radio/{self.label}.m3u8",
            "live_mp3": (f"/radio/{self.label}.live.mp3" if self.opts.live_mp3 else None),
            "dvr_window_s": self.opts.dvr_s, "codec": self.opts.codec,
        }

    def close(self):
        self._stop.set()
        if self._thread.ident is not None:        # only if the writer was started
            self._thread.join(timeout=2)
        for p in (self._proc, self._live):
            if p:
                try:
                    p.stdin.close()
                    p.wait(timeout=3)
                except (OSError, subprocess.TimeoutExpired):
                    try:
                        p.kill()
                    except OSError:
                        pass
        if self._archive:
            self._archive.close()


# ── HTTP ───────────────────────────────────────────────────────────────────────
class _RadioHTTP(SimpleHTTPRequestHandler):
    """Serves the HLS dir + `/` (player), `/stations.json`, `/healthz`, `/metrics`."""
    extensions_map = {
        **SimpleHTTPRequestHandler.extensions_map,
        ".m3u8": "application/vnd.apple.mpegurl", ".ts": "video/mp2t",
        ".m4s": "video/iso.segment", ".mp4": "video/mp4", ".mp3": "audio/mpeg",
    }

    def __init__(self, *a, server_ref=None, **k):
        self._ref = server_ref
        super().__init__(*a, **k)

    def log_message(self, *a):
        pass

    def _bytes(self, code, body, ctype):
        body = body.encode() if isinstance(body, str) else body
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-cache")
        self.end_headers()
        self.wfile.write(body)

    def end_headers(self):
        if self.path.endswith((".m3u8", ".mp3")):
            self.send_header("Cache-Control", "no-cache")
        super().end_headers()

    def do_GET(self):
        if self.path in ("/", "/index.html"):
            return self._bytes(200, PLAYER_HTML, "text/html; charset=utf-8")
        if self.path == "/stations.json":
            return self._bytes(200, json.dumps(self._ref.stations_info()), "application/json")
        if self.path == "/healthz":
            return self._bytes(200, json.dumps(self._ref.healthz()), "application/json")
        if self.path == "/metrics":
            return self._bytes(200, self._ref.metrics_text(), "text/plain; version=0.0.4")
        if self.path.startswith("/radio/"):
            self.path = self.path[len("/radio"):]
        return super().do_GET()


class RadioServer:
    """Routes captured frames to per-channel stations and serves them over HTTP."""

    def __init__(self, hls_dir, http_addr=None, only_channel=None, log=print, **opt_kw):
        self.hls_dir = hls_dir
        self.opts = _Opts(**opt_kw)
        self.only_channel = only_channel
        self.log = log
        self.started = time.time()
        self._stations = {}
        self._lock = threading.Lock()
        self._httpd = None
        os.makedirs(hls_dir, exist_ok=True)
        if http_addr:
            self._start_http(http_addr)

    def _start_http(self, addr):
        handler = partial(_RadioHTTP, server_ref=self, directory=self.hls_dir)
        self._httpd = ThreadingHTTPServer(addr, handler)
        threading.Thread(target=self._httpd.serve_forever, name="radio-http",
                         daemon=True).start()
        self.log(f"radio: HTTP on http://{addr[0]}:{addr[1]}/  (/, /stations.json, "
                 f"/healthz, /metrics)")

    def feed(self, frame, _addr=None):
        """Ingest one 17-byte AUDIO DeModFrame (from wiretap or a node tee)."""
        if len(frame) != FRAME_SIZE:
            return
        try:
            d = decode(frame)
        except ValueError:
            return
        dst, src = d["dst"], d["src"]
        if self.only_channel is not None and dst != self.only_channel:
            return
        with self._lock:
            st = self._stations.get(dst)
            if st is None:
                st = RadioStation(dst, self.hls_dir, self.opts, self.log)
                self._stations[dst] = st
                self.log(f"radio: new station {st.label}")
        st.feed(frame, src)

    def stations_info(self):
        with self._lock:
            return [s.info() for s in sorted(self._stations.values(), key=lambda s: s.dst)]

    def healthz(self):
        with self._lock:
            n = len(self._stations)
        return {"status": "healthy" if n else "idle", "stations": n,
                "uptime_s": round(time.time() - self.started, 1)}

    def metrics_text(self):
        infos = self.stations_info()
        active = len({p["src"] for i in infos for p in i["peers"]})
        lines = [
            "# HELP dcf_radio_stations Number of live stations.",
            "# TYPE dcf_radio_stations gauge",
            f"dcf_radio_stations {len(infos)}",
            "# HELP dcf_radio_active_peers Distinct peers seen across stations.",
            "# TYPE dcf_radio_active_peers gauge",
            f"dcf_radio_active_peers {active}",
            "# TYPE dcf_radio_uptime_seconds gauge",
            f"dcf_radio_uptime_seconds {round(time.time() - self.started, 1)}",
            "# TYPE dcf_radio_frames_total counter",
            "# TYPE dcf_radio_silence_frames_total counter",
            "# TYPE dcf_radio_dropped_frames_total counter",
        ]
        for i in infos:
            lbl = f'{{channel="{i["label"]}"}}'
            lines.append(f'dcf_radio_frames_total{lbl} {i["frames_real"]}')
            lines.append(f'dcf_radio_silence_frames_total{lbl} {i["frames_silence"]}')
            lines.append(f'dcf_radio_dropped_frames_total{lbl} {i["dropped"]}')
            lines.append(f'dcf_radio_station_peers{lbl} {i["num_peers"]}')
        return "\n".join(lines) + "\n"

    def close(self):
        if self._httpd:
            self._httpd.shutdown()
        with self._lock:
            for s in self._stations.values():
                s.close()


PLAYER_HTML = r"""<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>DCF Radio</title>
<style>
 body{font:15px system-ui,sans-serif;background:#0b0d10;color:#e6e6e6;margin:0;padding:24px}
 h1{font-size:20px;margin:0 0 4px} .dim{color:#8a8f98}
 .st{display:flex;gap:10px;align-items:center;padding:8px 10px;border:1px solid #222;border-radius:8px;margin:6px 0;cursor:pointer}
 .st:hover{border-color:#3a6} .on{border-color:#3a6;background:#10211a}
 .live{color:#e55;font-weight:bold} audio{width:100%;margin-top:14px}
</style></head><body>
<h1>◈ DCF Radio</h1><div class="dim">live channels — tune in, scrub back through the DVR window</div>
<div id="list"></div>
<audio id="player" controls></audio>
<div class="dim" id="now"></div>
<script src="https://cdn.jsdelivr.net/npm/hls.js@1"></script>
<script>
const audio=document.getElementById('player'),list=document.getElementById('list'),now=document.getElementById('now');
let hls=null;
function play(url,label){
  now.textContent='now playing: '+label+'  ('+url+')';
  document.querySelectorAll('.st').forEach(e=>e.classList.remove('on'));
  if(hls){hls.destroy();hls=null;}
  if(audio.canPlayType('application/vnd.apple.mpegurl')){audio.src=url;}
  else if(window.Hls&&Hls.isSupported()){hls=new Hls({liveSyncDuration:6});hls.loadSource(url);hls.attachMedia(audio);}
  audio.play().catch(()=>{});
}
async function refresh(){
  let s=[]; try{s=await (await fetch('/stations.json')).json();}catch(e){}
  list.innerHTML = s.length?'':'<div class="dim">no stations yet — waiting for audio…</div>';
  s.forEach(st=>{const d=document.createElement('div');d.className='st';
    const talking=st.peers.filter(p=>p.talking).map(p=>p.name).join(', ');
    d.innerHTML='<b>'+(st.name||st.label)+'</b> <span class="dim">'+st.num_peers+' peer(s)'+
      (talking?' · <span class="live">● '+talking+'</span>':' · idle')+' · '+Math.round(st.uptime_s)+'s</span>';
    d.onclick=()=>{play(st.playlist,st.name||st.label);d.classList.add('on');};list.appendChild(d);});
}
refresh(); setInterval(refresh,2000);
</script></body></html>"""
