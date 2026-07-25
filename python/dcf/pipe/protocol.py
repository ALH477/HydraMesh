# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-Pipe sender/receiver state machines + an in-memory driver.

Design (Documentation/DCF_PIPE_SPEC.md):
  * Control plane — the certified pipelab_core messages (OPEN/CREDIT/SACK/NACK/
    DONE/ABORT), carried over DCF frames in a real deployment.
  * Data plane — dumb `[session, seq, payload]` datagrams. Each chunk's payload
    is wrapped by the certified DCF-FEC message layer so byte corruption below
    the FEC budget heals forward with no round trip; a wholly dropped chunk is a
    gap the receiver NACKs and the sender retransmits.
  * Flow control — receiver-driven credit: the sender may only have `credit`
    chunks in flight; the receiver replenishes credit as it consumes.
  * Completion — the receiver reassembles, checks the whole-object FNV-1a from
    OPEN, and sends DONE (or ABORT with ABORT_CHECKSUM on mismatch).

The state machines are pure: they consume/emit messages and never touch a
socket, so they drive equally over UDP, a LoopbackTransport, or the in-memory
lossy channel used by the tests. `run_transfer` is the reference driver.
"""
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "MCP"))
import pipelab_core as pc  # noqa: E402
import feclab_core as fec  # noqa: E402


# ── link profiles ────────────────────────────────────────────────────────────
# A profile is just tuned defaults for (chunk_size, nparity, credit_window).
# The invariant and the control bytes are identical on every link; only the
# economics change. See DCF_PIPE_SPEC.md §8.
#
#   lan        — MTU-sized chunks, own FEC, deep window. ~10 Mb/s+.
#   hydramodem — the acoustic M-FSK link (~8-12 app-bytes/s). Chunks are small
#                so one loss costs seconds, not minutes; nparity=0 because the
#                modem's own convolutional FEC + interleaver already corrects
#                the channel (double-FEC just burns airtime, which is the one
#                truly scarce resource); window 2 because authorizing chunks the
#                link needs minutes to carry buys nothing.
#   sneakernet — bulk over a fast, reliable medium; huge chunks, no FEC.
PROFILES = {
    "lan":        {"chunk_size": 1400, "nparity": 16, "credit_window": 16},
    "hydramodem": {"chunk_size": 256,  "nparity": 0,  "credit_window": 2},
    "sneakernet": {"chunk_size": 8192, "nparity": 0,  "credit_window": 32},
}


def profile(name):
    """Tuned (chunk_size, nparity, credit_window) defaults for a link type."""
    if name not in PROFILES:
        raise ValueError(f"unknown profile {name!r}; have {sorted(PROFILES)}")
    return dict(PROFILES[name])


class PipeSender:
    """Sends one object. Call `messages_to_send()` for datagrams to emit, feed
    control messages back via `on_control`.

    `nparity=0` disables the per-chunk FEC wrap: use it when the link below
    already corrects errors (HydraModem's convolutional FEC + interleaver), so
    airtime isn't spent on redundant parity. Losslessness is unaffected — a
    chunk that fails the PHY's own check simply never arrives, and Φ's NACK
    path recovers it.
    """

    def __init__(self, session_id, data, chunk_size=1024, nparity=16):
        self.session_id = session_id
        self.data = bytes(data)
        self.chunk_size = chunk_size
        self.nparity = nparity
        self.checksum = pc.fnv1a32(self.data)
        self.n = pc.num_chunks(len(self.data), chunk_size)
        self.credit = 0
        self.sent = set()          # chunk seqs sent at least once
        self.acked = set()         # chunk seqs the receiver confirmed
        self.resend = []           # seqs explicitly NACKed
        self.done = False
        self.aborted = None
        self._opened = False

    def _chunk_payload(self, seq):
        raw = self.data[seq * self.chunk_size:(seq + 1) * self.chunk_size]
        if self.nparity == 0:
            return raw  # link below already corrects (e.g. HydraModem's conv FEC)
        # FEC-wrap so byte corruption in a delivered chunk self-heals.
        return fec.encode_message(raw, self.nparity)

    def open_message(self):
        self._opened = True
        return pc.pack_open(self.session_id, len(self.data), self.chunk_size, self.checksum)

    def on_control(self, buf):
        t = pc.pipe_msg_type(buf)
        if t == pc.PIPE_CREDIT:
            # Receiver-driven per-round send budget: the receiver sizes each
            # grant to cover a window of fresh chunks plus any it has NACKed,
            # so a burst of drops can't starve the retransmits.
            _, c = pc.unpack_credit(buf)
            self.credit = c
        elif t == pc.PIPE_SACK:
            _, base, bitmap = pc.unpack_sack(buf)
            for seq in range(self.n):
                if pc.sack_has(bitmap, base, seq):
                    self.acked.add(seq)
        elif t == pc.PIPE_NACK:
            _, missing = pc.unpack_nack(buf)
            for seq in missing:
                if seq not in self.acked:
                    self.resend.append(seq)
        elif t == pc.PIPE_DONE:
            self.done = True
        elif t == pc.PIPE_ABORT:
            _, reason = pc.unpack_abort(buf)
            self.aborted = reason

    def data_to_send(self):
        """Yield chunk datagrams permitted by the current credit window."""
        out = []
        # explicit retransmits first (they unblock the receiver's gaps)
        seen = set()
        while self.resend and self.credit > 0:
            seq = self.resend.pop(0)
            if seq in self.acked or seq in seen:
                continue
            seen.add(seq)
            out.append(pc.pack_chunk(self.session_id, seq, self._chunk_payload(seq)))
            self.sent.add(seq)
            self.credit -= 1
        # then fresh chunks
        for seq in range(self.n):
            if self.credit <= 0:
                break
            if seq in self.sent or seq in self.acked:
                continue
            out.append(pc.pack_chunk(self.session_id, seq, self._chunk_payload(seq)))
            self.sent.add(seq)
            self.credit -= 1
        return out

    def finished(self):
        return self.done or self.aborted is not None


class PipeReceiver:
    """Receives one object. Consumes OPEN + chunk datagrams; emits credit/acks/
    nacks and finally DONE or ABORT."""

    def __init__(self, credit_window=8, nparity=16):
        self.nparity = nparity  # must match the sender's; 0 = no per-chunk FEC
        self.session_id = None
        self.total_len = None
        self.chunk_size = None
        self.checksum = None
        self.n = None
        self.chunks = {}           # seq -> decoded payload
        self.credit_window = credit_window
        self.horizon = 0           # chunks authorized so far (the pull frontier)
        self.done = False
        self.aborted = None
        self.result = None

    def _base(self):
        base = 0
        while base in self.chunks:
            base += 1
        return base

    def on_open(self, buf):
        sid, total, cs, ck = pc.unpack_open(buf)
        self.session_id, self.total_len, self.chunk_size, self.checksum = sid, total, cs, ck
        self.n = pc.num_chunks(total, cs)

    def on_chunk(self, buf):
        sid, seq, payload = pc.unpack_chunk(buf)
        if self.session_id is not None and sid != self.session_id:
            return
        if seq in self.chunks:
            return
        if self.nparity == 0:
            raw = payload  # link below already corrected (or dropped) it
        else:
            try:
                raw, _ = fec.decode_message(payload)  # forward error correction
            except fec.FecError:
                return  # unrecoverable corruption -> a gap; NACK will refetch
        self.chunks[seq] = raw

    def control_messages(self):
        """Control to send back: credit replenishment, SACK, NACK for gaps, and
        DONE/ABORT at completion."""
        if self.session_id is None or self.done or self.aborted is not None:
            return []
        out = []
        # completion check
        if len(self.chunks) == self.n:
            obj = b"".join(self.chunks[i] for i in range(self.n))[:self.total_len]
            if pc.fnv1a32(obj) == self.checksum:
                self.result = obj
                self.done = True
                out.append(pc.pack_done(self.session_id))
            else:
                self.aborted = pc.ABORT_CHECKSUM
                out.append(pc.pack_abort(self.session_id, pc.ABORT_CHECKSUM))
            return out
        # Advance the authorization horizon by one window. Chunks in
        # [prev_horizon, horizon) are freshly authorized — the sender pulls them
        # with credit. Holes *below* prev_horizon were authorized a full round
        # ago and still haven't arrived, so they were genuinely lost: NACK them.
        # This distinguishes "in flight" from "dropped" by round, not position,
        # so a lost final chunk (no higher arrival to expose it) is still caught,
        # while chunks not yet due are never spuriously retransmitted.
        prev = self.horizon
        self.horizon = min(self.n, self.horizon + self.credit_window)
        fresh = self.horizon - prev
        holes = [s for s in range(prev) if s not in self.chunks]
        # Per-round send budget = fresh window + the retransmits requested, so a
        # burst of drops can never starve the retransmits (L4 flow-bound holds).
        out.append(pc.pack_credit(self.session_id, fresh + len(holes)))
        if holes:
            out.append(pc.pack_nack(self.session_id, holes[:255]))
        # cumulative SACK so the sender can retire acked chunks
        out.append(pc.pack_sack(self.session_id, self._base(), b""))
        return out

    def finished(self):
        return self.done or self.aborted is not None


def run_transfer(data, chunk_size=1024, nparity=16, credit_window=8,
                 drop=0.0, corrupt=0.0, seed=0, max_rounds=100000, profile_name=None):
    """Drive a sender->receiver transfer over an in-memory channel that drops
    whole datagrams with probability `drop` and flips a byte with probability
    `corrupt`. Returns (received_bytes, stats). Deterministic for a given seed.

    Below the FEC budget, `corrupt` is healed forward (no retransmit); whole
    drops are recovered by NACK. The transfer completes only when the receiver's
    whole-object checksum matches.
    """
    import random
    if profile_name:  # a link profile supplies the tuned defaults
        p = profile(profile_name)
        chunk_size, nparity, credit_window = p["chunk_size"], p["nparity"], p["credit_window"]
    rng = random.Random(seed)
    sender = PipeSender(1, data, chunk_size, nparity)
    receiver = PipeReceiver(credit_window, nparity)
    stats = {"chunks_sent": 0, "dropped": 0, "corrupted": 0, "rounds": 0, "retransmits": 0}

    def via_channel(datagram):
        """Return the (possibly corrupted) datagram, or None if dropped."""
        if rng.random() < drop:
            stats["dropped"] += 1
            return None
        b = bytearray(datagram)
        if corrupt > 0 and rng.random() < corrupt and len(b) > pc.CHUNK_HDR_LEN:
            # flip one payload byte (header stays intact so it still routes)
            i = rng.randrange(pc.CHUNK_HDR_LEN, len(b))
            b[i] ^= 0xFF
            stats["corrupted"] += 1
        return bytes(b)

    # OPEN is control (assume the control plane is reliable, like DCF-Mesh's).
    receiver.on_open(sender.open_message())

    for _ in range(max_rounds):
        stats["rounds"] += 1
        # receiver -> sender control (reliable)
        for msg in receiver.control_messages():
            sender.on_control(msg)
        if receiver.finished():
            break
        # sender -> receiver data (lossy)
        datagrams = sender.data_to_send()
        for dg in datagrams:
            stats["chunks_sent"] += 1
            delivered = via_channel(dg)
            if delivered is not None:
                receiver.on_chunk(delivered)
        if sender.finished() and receiver.finished():
            break
    else:
        raise RuntimeError("transfer did not converge")

    stats["retransmits"] = stats["chunks_sent"] - receiver.n if receiver.n else 0
    return receiver.result, stats
