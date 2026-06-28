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


class Csma(Mac):
    """Carrier-sense multiple access with exponential backoff, for low-traffic / ad-hoc
    nodes that don't want assigned slots. A node transmits opportunistically; on a
    detected collision the caller calls `backoff(attempt)` and retries after the
    returned delay. NB: real carrier-sense needs the live medium (the gateway/PHY must
    report channel-busy); on the loopback/file medium there are no true collisions, so
    this behaves as send-on-ready. `rng` is injectable for deterministic tests."""
    mode = "csma"

    def __init__(self, slot_dur=0.5, max_backoff_slots=8, rng=None):
        import random
        self.slot_dur = float(slot_dur)
        self.max_backoff_slots = int(max_backoff_slots)
        self._rng = rng or random.Random()

    def next_tx(self, node_slot, now, epoch=0.0):
        return now                                  # send as soon as there's data

    def backoff(self, attempt):
        """Randomized exponential backoff delay (seconds) for retry `attempt` (0-based)."""
        window = min(2 ** attempt, self.max_backoff_slots)
        return self._rng.randint(0, max(0, window - 1)) * self.slot_dur


class Fdma(Mac):
    """Frequency-division: each node sits on a distinct HydraModem tone channel, so nodes
    transmit concurrently and the gateway runs one decoder per channel. Channel k uses
    base_freq = base0 + k*spacing; freqs stay integer multiples of baud (orthogonal).
    No time gating (concurrent) — the channel separation prevents collisions."""
    mode = "fdma"

    def __init__(self, num_channels, base0=2000, spacing=2000, tone_spacing=1000,
                 baud=1000, n_tones=2, sample_rate=48000):
        if num_channels < 1:
            raise ValueError("num_channels must be >= 1")
        top = base0 + (num_channels - 1) * spacing + tone_spacing * (n_tones - 1)
        if top >= sample_rate / 2:
            raise ValueError(f"top tone {top} Hz exceeds Nyquist {sample_rate/2:.0f} Hz "
                             "(reduce num_channels/spacing)")
        self.num_channels = int(num_channels)
        self.base0, self.spacing = base0, spacing
        self.tone_spacing, self.baud, self.n_tones = tone_spacing, baud, n_tones

    def channel_of(self, node_id):
        return node_id % self.num_channels

    def profile_of(self, channel):
        """HydraModem profile kwargs for a channel (-> HydraTransport / frame_tx args)."""
        return {"base_freq": self.base0 + channel * self.spacing,
                "tone_spacing": self.tone_spacing, "baud": self.baud,
                "n_tones": self.n_tones}


def make_mac(cfg):
    """Build a Mac from a config dict. Unknown modes raise."""
    cfg = cfg or {}
    mode = cfg.get("mode", "tdma")
    if mode == "dedicated":
        return Dedicated()
    if mode == "tdma":
        return Tdma(num_slots=cfg.get("num_slots", 1),
                    slot_dur=cfg.get("slot_dur", 1.0),
                    guard=cfg.get("guard", 0.05))
    if mode == "csma":
        return Csma(slot_dur=cfg.get("slot_dur", 0.5),
                    max_backoff_slots=cfg.get("max_backoff_slots", 8))
    if mode == "fdma":
        return Fdma(num_channels=cfg.get("num_channels", 1),
                    base0=cfg.get("base0", 2000), spacing=cfg.get("spacing", 2000),
                    tone_spacing=cfg.get("tone_spacing", 1000),
                    baud=cfg.get("baud", 1000), n_tones=cfg.get("n_tones", 2))
    raise ValueError(f"unknown mac mode {mode!r} (tdma|dedicated|csma|fdma)")
