# SPDX-License-Identifier: LGPL-3.0-only
"""Wiretap — a passive on-path observer for the DCF UDP wire.

The DCF wire is **plaintext by design** (encryption-free for EAR/ITAR export
compliance — see ``Documentation/Specs/export_compliance.markdown``). The flip side
is that *any* on-path party — a malicious router, a compromised relay, anyone with a
span port — can read everything: mesh membership, topology + per-peer RTT (REPORT),
who is master (ROLE), and message contents. This module makes that exposure concrete.

``Wiretap`` is a transparent UDP relay: it binds a port, **decodes and records**
every datagram it sees (holding no keys, participating in nothing), then forwards it
unchanged to the real destination. Point two nodes' peer addresses at the wiretap and
it reconstructs the whole mesh from the plaintext. See
``Documentation/DCF_SECURITY_EXPOSURE.md`` and ``python/tests/test_eavesdrop_leak.py``.

The mitigation is to run DCF inside a WireGuard tunnel (or the operator's own,
export-compliant crypto) layered *under* this UDP socket — never inside the protocol.
``external_wrap`` below is a deliberately trivial stand-in used only to demonstrate
that, once the bytes are wrapped outside DCF, the same wiretap recovers nothing.
"""

import logging
import os
import socket
import sys
import threading

from .proto import (
    ProtoMessage,
    MSG_PING,
    MSG_PONG,
    MSG_MESH,
    MSG_TEXT_DCF,
    MSG_POSITION,
)

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))
import meshlab_core as mesh  # noqa: E402

log = logging.getLogger("dcf.wiretap")

_TYPE_NAMES = {
    MSG_PING: "PING", MSG_PONG: "PONG", MSG_MESH: "MESH",
    MSG_TEXT_DCF: "TEXT", MSG_POSITION: "POSITION",
}


def describe(msg):
    """Human-readable, fully-decoded summary of a captured ProtoMessage.

    This is the leak: with zero credentials we recover the adapter contents —
    mesh topology, role assignments, and message payloads — straight from the wire.
    """
    name = _TYPE_NAMES.get(msg.msg_type, "type%d" % msg.msg_type)
    if msg.msg_type == MSG_MESH:
        mtype = mesh.mesh_msg_type(msg.payload)
        if mtype == mesh.MESH_REPORT:
            try:
                nid, peers = mesh.unpack_report(msg.payload)
                view = ", ".join("peer%d=%s/%dms" % (p, _st(s), r) for p, s, r in peers)
                return "MESH/REPORT node=%d peers=[%s]" % (nid, view)
            except ValueError:
                pass
        elif mtype == mesh.MESH_ROLE:
            try:
                nid, role, master = mesh.unpack_role(msg.payload)
                return "MESH/ROLE node=%d role=%s master=%d" % (nid, _role(role), master)
            except ValueError:
                pass
        return "MESH/?? %r" % (msg.payload,)
    if msg.payload:
        # Best-effort: many adapters carry UTF-8 text; show it if printable.
        try:
            txt = msg.payload.decode("utf-8")
            if txt.isprintable():
                return "%s text=%r" % (name, txt)
        except UnicodeDecodeError:
            pass
        return "%s payload=%s" % (name, msg.payload.hex())
    return name


def _st(s):
    return {mesh.HEALTHY: "healthy", mesh.DEGRADED: "degraded"}.get(s, "unreachable")


def _role(r):
    return {mesh.MASTER: "master", mesh.RELAY: "relay"}.get(r, "leaf")


def external_wrap(data, key=0x5A):
    """Stand-in for an external transport encryptor (e.g. WireGuard).

    A single-byte XOR is obviously NOT real crypto — it is only here to model
    "the bytes on the wire are no longer a DCF ProtoMessage". In production this is
    WireGuard, applied beneath the UDP socket, entirely outside DCF. Do NOT add this
    (or any crypto) to the DCF core wire path.
    """
    return bytes(b ^ key for b in data)


# external_wrap is its own inverse (XOR), so unwrap == wrap.
external_unwrap = external_wrap


class Wiretap:
    """Transparent capturing UDP relay. Forwards ``bind`` -> ``forward``.

    ``forward`` may be ``None`` for a pure passive listener (bind + capture only,
    no relay) — used by ``dcf-rec listen`` to record audio sent at a bound port.
    """

    def __init__(self, bind, forward=None, on_capture=None, label="wiretap"):
        self.forward = (forward[0], int(forward[1])) if forward else None
        self.label = label
        self.on_capture = on_capture
        self.captures = []                     # list of (raw_bytes, src_addr)
        self._sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self._sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        # Large receive buffer so bursty audio (a station's many small frames) isn't
        # dropped by the kernel before the recv loop drains it.
        try:
            self._sock.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 4 * 1024 * 1024)
        except OSError:
            pass
        self._sock.bind((bind[0], int(bind[1])))
        self._sock.settimeout(0.5)
        self._running = False
        self._thread = None

    def start(self):
        self._running = True
        self._thread = threading.Thread(target=self._loop, name=self.label, daemon=True)
        self._thread.start()

    def stop(self):
        self._running = False
        if self._thread:
            self._thread.join(timeout=2)
        try:
            self._sock.close()
        except OSError:
            pass

    def _loop(self):
        while self._running:
            try:
                data, src = self._sock.recvfrom(65536)
            except socket.timeout:
                continue
            except OSError:
                break
            self.captures.append((data, src))
            try:
                msg = ProtoMessage.deserialize(data)
                summary = describe(msg)
            except ValueError:
                summary = "<undecodable: %d bytes>" % len(data)
            log.info("[%s] %s -> %s : %s", self.label, src, self.forward, summary)
            if self.on_capture:
                self.on_capture(data, src, summary)
            # Forward unchanged — a passive tap is transparent to the mesh. With no
            # forward configured this is a pure listener (no relay).
            if self.forward is not None:
                try:
                    self._sock.sendto(data, self.forward)
                except OSError:
                    pass

    def decoded(self):
        """All captures that decode as a DCF ProtoMessage (the recovered plaintext)."""
        out = []
        for data, src in self.captures:
            try:
                out.append(ProtoMessage.deserialize(data))
            except ValueError:
                continue
        return out
