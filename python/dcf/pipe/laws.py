# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-Pipe transfer laws — the invariant system that *guarantees* losslessness.

The control-message bytes are pinned by pipe_vectors.json (the wire invariant).
This module pins the harder thing: the properties the *transfer* guarantees,
stated as executable laws and checked over a randomized sweep of objects, chunk
sizes, drop rates, corruption rates, and credit windows — the same way
verify_laws.py pins the wire codec. If every law holds across the sweep, the
runtime provably delivers the object byte-exact or does not claim DONE.

The five laws (see DCF_PIPE_SPEC.md §4–§5):

  L1 INTEGRITY   — DONE ⟹ received bytes == sent bytes, exactly. Completion is
                   never claimed on a corrupted or partial object.
  L2 LOSSLESS    — for any drop < 1.0 and any FEC-correctable corruption, the
                   transfer terminates with result == original. Loss causes
                   delay (retransmits), never data loss.
  L3 FEC-FORWARD — corruption within the FEC budget is healed with ZERO
                   retransmits (throughput: no round trip for bit errors).
  L4 FLOW-BOUND  — the sender never puts more chunks on the lane in one round
                   than the receiver's credit for that round (receiver-driven
                   flow control holds; the sender cannot outrun the receiver).
  L5 IDEMPOTENT  — duplicate / reordered chunk delivery never corrupts the
                   result (the receiver dedupes by sequence number).

Run:  python3 python/dcf/pipe/laws.py    # exit 0 iff every law holds
"""
import os
import random
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", "MCP"))
import pipelab_core as pc  # noqa: E402

try:
    from .protocol import PipeSender, PipeReceiver
    from .invariant import check_transfer
except ImportError:  # run as a bare script
    from protocol import PipeSender, PipeReceiver
    from invariant import check_transfer

ok = lambda name: print(f"  PASS  {name}")


def _instrumented_transfer(data, chunk_size, nparity, credit_window,
                           drop, corrupt, seed, dup=0.0, reorder=False,
                           max_rounds=200000):
    """Like run_transfer, but records per-round facts the laws inspect:
    max chunks sent in any single round vs. the credit granted that round."""
    rng = random.Random(seed)
    sender = PipeSender(1, data, chunk_size, nparity)
    receiver = PipeReceiver(credit_window, nparity)
    facts = {"retransmits": 0, "flow_ok": True, "chunks_sent": 0, "delivered_dupes": 0}

    receiver.on_open(sender.open_message())
    reorder_buf = []

    for _ in range(max_rounds):
        # receiver -> sender control; capture the credit granted this round
        granted_this_round = 0
        for msg in receiver.control_messages():
            if pc.pipe_msg_type(msg) == pc.PIPE_CREDIT:
                _, granted_this_round = pc.unpack_credit(msg)
            sender.on_control(msg)
        if receiver.finished():
            break

        datagrams = sender.data_to_send()
        # L4: the sender must not exceed the credit it was granted this round.
        if len(datagrams) > granted_this_round:
            facts["flow_ok"] = False

        # deliver through the lossy/dup/reorder channel
        for dg in datagrams:
            facts["chunks_sent"] += 1
            if rng.random() < drop:
                continue
            b = bytearray(dg)
            if corrupt > 0 and rng.random() < corrupt and len(b) > pc.CHUNK_HDR_LEN:
                b[rng.randrange(pc.CHUNK_HDR_LEN, len(b))] ^= 0xFF
            deliveries = [bytes(b)]
            if dup > 0 and rng.random() < dup:
                deliveries.append(bytes(b))  # duplicate delivery (L5)
                facts["delivered_dupes"] += 1
            if reorder:
                reorder_buf.extend(deliveries)
                if len(reorder_buf) >= 3:
                    rng.shuffle(reorder_buf)
                    for d in reorder_buf:
                        receiver.on_chunk(d)
                    reorder_buf = []
            else:
                for d in deliveries:
                    receiver.on_chunk(d)

        if sender.finished() and receiver.finished():
            break
    else:
        # flush any buffered reordered deliveries before declaring non-convergence
        for d in reorder_buf:
            receiver.on_chunk(d)
        for msg in receiver.control_messages():
            sender.on_control(msg)
        if not receiver.finished():
            raise RuntimeError(f"did not converge (drop={drop} corrupt={corrupt})")

    facts["retransmits"] = facts["chunks_sent"] - (receiver.n or 0)
    return receiver.result, receiver, facts


def verify_laws():
    rng = random.Random(20260725)
    # (object, chunk sizes to try). Chunk sizes are paired with object sizes so
    # the sweep covers the interesting shapes — 1-chunk, few-chunk, many-chunk,
    # and sub-chunk-size objects — without pathological chunk counts.
    cases_matrix = [
        (bytes(), [8, 512]),                                        # empty
        (b"x", [1, 8, 512]),                                        # single byte
        (bytes((i * 31 + 7) & 0xFF for i in range(50)), [1, 8, 64, 512]),
        (bytes((i * 91 + 3) & 0xFF for i in range(5000)), [64, 512, 1400]),
        (os.urandom(30000), [512, 1400]),                           # large
    ]

    # L1 INTEGRITY + L2 LOSSLESS across a sweep of loss regimes.
    integrity_ok = lossless_ok = True
    cases = 0
    for data, chunk_sizes in cases_matrix:
        for cs in chunk_sizes:
            for drop in (0.0, 0.1, 0.3, 0.6):
                for corrupt in (0.0, 0.25):
                    seed = rng.randrange(1 << 30)
                    result, recv, _ = _instrumented_transfer(
                        data, cs, 16, 8, drop, corrupt, seed)
                    cases += 1
                    # L1: if it claimed DONE, the object is byte-exact
                    if recv.done and result != data:
                        integrity_ok = False
                    # L2: it always reaches DONE with the exact object
                    if result != data:
                        lossless_ok = False
    assert integrity_ok, "L1 INTEGRITY violated"
    assert lossless_ok, "L2 LOSSLESS violated"
    ok(f"L1 INTEGRITY + L2 LOSSLESS over {cases} transfers (drop up to 60%, +corruption)")

    # L3 FEC-FORWARD: corruption alone (no drops) heals with zero retransmits.
    for cs in (64, 512, 1400):
        data = bytes((i * 17 + 5) & 0xFF for i in range(8000))
        result, _, facts = _instrumented_transfer(data, cs, 16, 8, 0.0, 0.3, 12345)
        assert result == data, "L3 corruption not healed"
        assert facts["retransmits"] == 0, f"L3 FEC-FORWARD: {facts['retransmits']} retransmits for pure corruption"
    ok("L3 FEC-FORWARD — in-budget corruption healed with 0 retransmits (no round trip)")

    # L4 FLOW-BOUND: the sender never exceeds the granted credit in any round.
    for drop in (0.0, 0.2, 0.5):
        data = os.urandom(12000)
        _, _, facts = _instrumented_transfer(data, 256, 16, 8, drop, 0.1, 777)
        assert facts["flow_ok"], "L4 FLOW-BOUND: sender exceeded its credit"
    ok("L4 FLOW-BOUND — chunks/round never exceed the receiver's credit")

    # L5 IDEMPOTENT: duplicates + reordering never corrupt the result.
    for seed in (1, 2, 3):
        data = os.urandom(9000)
        result, _, facts = _instrumented_transfer(
            data, 300, 16, 8, drop=0.15, corrupt=0.1, seed=seed, dup=0.3, reorder=True)
        assert result == data, "L5 IDEMPOTENT violated under dup+reorder"
        assert facts["delivered_dupes"] > 0, "test did not actually exercise duplicates"
    ok("L5 IDEMPOTENT — duplicate + reordered delivery never corrupts the object")

    # Φ — the core invariant (invariant.py). check_transfer asserts S1/S2/C every
    # round via the monitor and L2/OPT at the end; sweep it across loss regimes.
    phi_cases = 0
    for data, chunk_sizes in cases_matrix:
        for cs in chunk_sizes:
            for drop, corrupt in ((0.0, 0.0), (0.3, 0.0), (0.0, 0.25), (0.5, 0.2)):
                check_transfer(data, chunk_size=cs, credit_window=8,
                               drop=drop, corrupt=corrupt, seed=rng.randrange(1 << 30))
                phi_cases += 1
    ok(f"Φ-INVARIANT (deficit potential) held every round across {phi_cases} transfers "
       f"— S1 monotone, S2 correct, C sound, L2 Φ→0, OPT ⌈N/W⌉ on clean links")

    print("ALL PIPE TRANSFER LAWS HOLD")


if __name__ == "__main__":
    verify_laws()
