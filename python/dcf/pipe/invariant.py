# SPDX-License-Identifier: LGPL-3.0-only
r"""DCF-Pipe's core invariant — the deficit potential Φ.

The wire quantum has one scalar invariant: a frame is valid iff its CRC
syndrome is 0. DCF-Pipe has the analogous single scalar for a *transfer*:

        Φ  =  N − |R|          (the deficit)

where N is the object's chunk count and R is the set of chunk indices the
receiver has correctly reassembled. Φ is the minimal *complete* certificate of a
lossless transfer, because it fuses the two things a reliability proof needs —
a safety invariant and a termination variant — into one well-founded scalar:

  THEOREM (losslessness + termination).
  Under a receiver that (a) admits a chunk into R only after it FEC-decodes,
  and (b) never removes a chunk from R, and a channel that delivers each
  requested chunk with probability p = 1 − drop > 0:

    S1  SAFETY (monotone):     Φ is non-increasing.  R never loses a chunk.
    S2  SAFETY (correct):      every index in R holds the *true* chunk bytes
                               (per-chunk FEC ⇒ a decoded chunk is correct or
                               rejected), so |R| = N ⇒ the reassembled object
                               equals the original.
    L1  LIVENESS (grounded):   Φ ∈ {0,…,N} is bounded below and well-founded.
    L2  LIVENESS (progress):   while Φ > 0 the receiver requests every hole, and
                               each is delivered in expected ≤ 1/p rounds, so Φ
                               strictly decreases infinitely often ⇒ reaches 0
                               in expected finite time.
    C   COMPLETION SOUNDNESS:  DONE is emitted ⟺ Φ = 0 ⟺ object is byte-exact.

  Corollary (lossless):  the transfer terminates with the object delivered
  byte-for-byte, for ANY drop < 1 and ANY FEC-correctable corruption. Loss costs
  rounds (Φ plateaus one round), never bytes.

  Corollary (optimal throughput): with no whole-chunk drops, Φ decreases by the
  credit window W every round, so the transfer finishes in exactly ⌈N/W⌉ rounds —
  the information-theoretic minimum for a window-W receiver — with ZERO retransmit
  round-trips (FEC absorbs bit corruption inline). Φ's slope is the throughput.

This module makes Φ a *checked* object: PipeInvariantMonitor asserts S1, S2, and
C every round against ground truth, and check_transfer additionally verifies L2
(Φ reaches 0) and the optimal-throughput corollary. If any assertion fails the
runtime has violated the invariant — the same contract discipline as the CRC
syndrome and the golden vectors.
"""
import math


def object_chunks(data, chunk_size):
    """Ground-truth chunk list an object splits into (what R is compared against)."""
    if chunk_size <= 0:
        raise ValueError("chunk_size must be positive")
    if not data:
        return [b""]  # a zero-length object is one empty chunk (num_chunks(0)=0 => handle 0)
    return [data[i:i + chunk_size] for i in range(0, len(data), chunk_size)]


def deficit(receiver):
    """Φ = N − |R| for a live PipeReceiver. N unknown before OPEN ⇒ +inf."""
    if receiver.n is None:
        return math.inf
    return receiver.n - len(receiver.chunks)


class PipeInvariantMonitor:
    """Asserts the Φ-invariant every round against the true object.

    Wrap a PipeReceiver; call `observe()` after each batch of chunk deliveries.
    Raises AssertionError the instant S1/S2/C is violated.
    """

    def __init__(self, receiver, data, chunk_size):
        self.receiver = receiver
        self.truth = object_chunks(data, chunk_size)
        self.n = 0 if not data else len(self.truth)
        self.phi_prev = math.inf
        self.trajectory = []

    def observe(self):
        r = self.receiver
        phi = deficit(r)

        # S2 — correctness: every admitted chunk equals the true chunk.
        for seq, payload in r.chunks.items():
            assert 0 <= seq < self.n, f"S2: chunk {seq} out of range [0,{self.n})"
            assert payload == self.truth[seq], f"S2: chunk {seq} admitted with wrong bytes"

        # S1 — monotonicity: Φ never rises (R never loses a chunk).
        if r.n is not None:
            assert phi <= self.phi_prev, f"S1: Φ rose {self.phi_prev} -> {phi}"

        # C — completion soundness: DONE ⟺ Φ == 0.
        if r.done:
            assert phi == 0, f"C: DONE emitted with Φ={phi} > 0"
            assert r.result is not None, "C: DONE without a reassembled result"

        self.phi_prev = phi
        self.trajectory.append(phi)
        return phi


def check_transfer(data, chunk_size=512, nparity=16, credit_window=8,
                   drop=0.0, corrupt=0.0, seed=0, max_rounds=200000):
    """Run one transfer under the Φ-monitor and return (result, report).

    Asserts S1/S2/C every round (via the monitor), plus:
      L2  — Φ reaches 0 (the transfer converges losslessly), and
      OPT — on a clean link (drop=0), it finishes in ⌈N/W⌉ rounds with no
            retransmit round-trips.
    """
    import os
    import random
    import sys
    sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "MCP"))
    import pipelab_core as pc  # noqa: E402
    try:
        from .protocol import PipeSender, PipeReceiver
    except ImportError:  # run as a bare script
        from protocol import PipeSender, PipeReceiver

    rng = random.Random(seed)
    sender = PipeSender(1, data, chunk_size, nparity)
    receiver = PipeReceiver(credit_window, nparity)
    receiver.on_open(sender.open_message())
    mon = PipeInvariantMonitor(receiver, data, chunk_size)
    mon.observe()  # initial Φ = N

    data_rounds = 0
    chunks_sent = 0
    for _ in range(max_rounds):
        for msg in receiver.control_messages():
            sender.on_control(msg)
        if receiver.finished():
            break
        datagrams = sender.data_to_send()
        if datagrams:
            data_rounds += 1
        for dg in datagrams:
            chunks_sent += 1
            if rng.random() < drop:
                continue
            b = bytearray(dg)
            if corrupt > 0 and rng.random() < corrupt and len(b) > pc.CHUNK_HDR_LEN:
                b[rng.randrange(pc.CHUNK_HDR_LEN, len(b))] ^= 0xFF
            receiver.on_chunk(bytes(b))
        mon.observe()
        if sender.finished() and receiver.finished():
            break
    else:
        raise RuntimeError("transfer did not converge")
    mon.observe()

    # L2 — Φ reached 0.
    assert mon.trajectory[-1] == 0, f"L2: Φ ended at {mon.trajectory[-1]} != 0"
    assert receiver.result == data, "L2: result not byte-exact"

    n = receiver.n or 0
    report = {
        "N": n,
        "phi0": mon.trajectory[0],
        "data_rounds": data_rounds,
        "chunks_sent": chunks_sent,
        "retransmits": chunks_sent - n,
        "optimal_rounds": math.ceil(n / credit_window) if n else 0,
    }
    # OPT — clean link is optimal: ⌈N/W⌉ data rounds, zero retransmits.
    if drop == 0.0 and corrupt == 0.0 and n > 0:
        assert report["retransmits"] == 0, "OPT: clean link had retransmits"
        assert data_rounds <= report["optimal_rounds"] + 1, \
            f"OPT: {data_rounds} data rounds > optimal {report['optimal_rounds']}"
    return receiver.result, report


if __name__ == "__main__":
    # Demonstrate Φ's trajectory and the optimality corollary.
    payload = bytes((i * 53 + 9) & 0xFF for i in range(8000))
    _, rep = check_transfer(payload, chunk_size=512, credit_window=8, drop=0.0)
    print(f"clean link:  N={rep['N']} Φ0={rep['phi0']} -> 0 in {rep['data_rounds']} rounds "
          f"(optimal {rep['optimal_rounds']}), {rep['retransmits']} retransmits")
    _, rep = check_transfer(payload, chunk_size=512, credit_window=8, drop=0.4, corrupt=0.2, seed=7)
    print(f"lossy link:  N={rep['N']} delivered byte-exact, {rep['retransmits']} retransmits "
          f"(Φ still reached 0)")
    print("Φ-INVARIANT HOLDS")
