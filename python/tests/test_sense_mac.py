# SPDX-License-Identifier: LGPL-3.0-only
"""DCF-MAC: TDMA slot assignment is collision-free (distinct nodes -> disjoint windows),
`due()` is true only inside the guarded slot, and `next_tx` advances to the slot start."""
import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

try:
    from dcf.sense import mac as macmod
    HAVE = True
except Exception:
    HAVE = False


@unittest.skipUnless(HAVE, "dcf.sense.mac required")
class TestTdma(unittest.TestCase):
    def test_slot_assignment_wraps(self):
        m = macmod.Tdma(num_slots=4, slot_dur=1.0, guard=0.1)
        self.assertEqual([m.slot_of(i) for i in range(6)], [0, 1, 2, 3, 0, 1])

    def test_windows_disjoint(self):
        m = macmod.Tdma(num_slots=5, slot_dur=2.0, guard=0.2)
        wins = [m.window(s) for s in range(5)]
        for (s0, e0), (s1, e1) in zip(wins, wins[1:]):
            self.assertLessEqual(e0, s1, "adjacent slots must not overlap")

    def test_no_collision_across_nodes(self):
        # every distinct slot's window is disjoint from every other -> shared line safe
        m = macmod.Tdma(num_slots=4, slot_dur=1.0, guard=0.05)
        wins = [m.window(m.slot_of(0x1000 + i)) for i in range(4)]
        for i in range(len(wins)):
            for j in range(i + 1, len(wins)):
                a, b = wins[i], wins[j]
                self.assertTrue(b[0] >= a[1] or a[0] >= b[1], "windows overlap")

    def test_due_only_in_window(self):
        m = macmod.Tdma(num_slots=3, slot_dur=1.0, guard=0.1)
        self.assertFalse(m.due(1, 0.05))    # before slot 1
        self.assertTrue(m.due(1, 1.5))      # inside slot 1 (1.1..1.9)
        self.assertFalse(m.due(1, 2.5))     # slot 2

    def test_next_tx_advances_to_slot_start(self):
        m = macmod.Tdma(num_slots=3, slot_dur=1.0, guard=0.1)
        self.assertAlmostEqual(m.next_tx(2, 0.0), 2.1, places=6)   # slot 2 opens at 2.1
        self.assertAlmostEqual(m.next_tx(0, 0.5), 3.1, places=6)   # slot 0 next cycle

    def test_dedicated_always_due(self):
        m = macmod.make_mac({"mode": "dedicated"})
        self.assertTrue(m.due(0, 123.4))

    def test_bad_guard_rejected(self):
        with self.assertRaises(ValueError):
            macmod.Tdma(num_slots=2, slot_dur=1.0, guard=0.6)


if __name__ == "__main__":
    unittest.main()
