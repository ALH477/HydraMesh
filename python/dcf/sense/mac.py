# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF-MAC — configurable media access for DCF-Sense over a shared wired medium.

HydraModem is a physical layer with no media access, so many nodes sharing one line
would collide. This adds a thin scheduling policy *above* the transport (it gates
*when* a node may transmit; it never touches the PHY bytes):

  dedicated  — each node has its own line -> no contention (always free to send).
  tdma       — a superframe of `num_slots` slots; a node transmits only in its slot,
               so a shared line never collides. `epoch` is the superframe t0, learned
               from the gateway BEACON in a real deployment.

`fdma` (per-node tone profiles) and `csma` (listen-before-talk) come in Phase 2.
"""


class Mac:
    """Base policy. `due()` says whether a node's slot may transmit at `now`."""
    mode = "base"

    def slot_of(self, node_id):
        return 0

    def due(self, node_slot, now, epoch=0.0):
        return True

    def next_tx(self, node_slot, now, epoch=0.0):
        return now


class Dedicated(Mac):
    """Each node on its own line — no contention."""
    mode = "dedicated"


class Tdma(Mac):
    """Time-division multiple access over one shared line.

    Superframe = `num_slots` slots of `slot_dur` seconds; `guard` trims each slot's
    edges so neighbouring nodes never overlap despite clock skew (HydraModem's timing
    recovery tolerates the in-slot offset)."""
    mode = "tdma"

    def __init__(self, num_slots, slot_dur=1.0, guard=0.05):
        if num_slots < 1:
            raise ValueError("num_slots must be >= 1")
        if guard * 2 >= slot_dur:
            raise ValueError("guard*2 must be < slot_dur")
        self.num_slots = int(num_slots)
        self.slot_dur = float(slot_dur)
        self.guard = float(guard)
        self.frame_dur = self.num_slots * self.slot_dur

    def slot_of(self, node_id):
        return node_id % self.num_slots

    def window(self, slot, cycle=0, epoch=0.0):
        """Guarded (start, end) of `slot` in superframe `cycle`."""
        base = epoch + cycle * self.frame_dur + slot * self.slot_dur
        return base + self.guard, base + self.slot_dur - self.guard

    def due(self, node_slot, now, epoch=0.0):
        t = (now - epoch) % self.frame_dur
        lo = node_slot * self.slot_dur + self.guard
        hi = (node_slot + 1) * self.slot_dur - self.guard
        return lo <= t < hi

    def next_tx(self, node_slot, now, epoch=0.0):
        """Next instant the node's (guarded) slot opens at or after `now`."""
        cycle = int((now - epoch) // self.frame_dur)
        for c in (cycle, cycle + 1):
            start, _ = self.window(node_slot, c, epoch)
            if start >= now - 1e-9:
                return start
        return self.window(node_slot, cycle + 1, epoch)[0]


def make_mac(cfg):
    """Build a Mac from {mode, num_slots, slot_dur, guard}. Unknown modes raise."""
    mode = (cfg or {}).get("mode", "tdma")
    if mode == "dedicated":
        return Dedicated()
    if mode == "tdma":
        return Tdma(num_slots=cfg.get("num_slots", 1),
                    slot_dur=cfg.get("slot_dur", 1.0),
                    guard=cfg.get("guard", 0.05))
    raise ValueError(f"unknown mac mode {mode!r} (tdma|dedicated; fdma/csma in Phase 2)")
