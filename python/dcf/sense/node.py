# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF-Sense sensor node: read sensors -> encode DCF-Sense readings -> transmit in the
node's MAC slot over any DCF transport (HydraModem, audio, loopback, ...). One reading
is one bare DeModFrame (src_id = node_id)."""
import threading
import time

from . import schema


class SensorNode:
    """`read(node_id)` returns the current readings as a dict {sensor_name|id: value}
    or an iterable of (sensor_type, value). The node transmits each reading as one
    bare DeModFrame, only inside its MAC slot."""

    def __init__(self, node_id, transport, mac, read, *, gw_channel=0xFFFF,
                 cadence=1.0, epoch=0.0):
        self.node_id = int(node_id)
        self.transport = transport
        self.mac = mac
        self.read = read
        self.gw = gw_channel
        self.cadence = cadence
        self.epoch = epoch
        self.slot = mac.slot_of(self.node_id)
        self.seq = 0
        self.sent = 0
        self._run = False
        self._th = None

    def _readings(self):
        r = self.read(self.node_id)
        if isinstance(r, dict):
            return [(k if isinstance(k, int) else schema.NAME_TO_ID[k], v)
                    for k, v in r.items()]
        return list(r)

    def report_once(self, ts_us=0):
        """Encode + transmit the current readings (one bare frame each). Returns count."""
        n = 0
        for sensor_type, value in self._readings():
            frame = schema.encode_reading(self.node_id, sensor_type, value,
                                          dst=self.gw, seq=self.seq, ts_us=ts_us)
            self.transport.send(frame)
            self.seq = (self.seq + 1) & 0xFFFF
            self.sent += 1
            n += 1
        return n

    def run(self, cycles=None):
        """Slot-timed loop: wait for this node's MAC slot, report, repeat at `cadence`."""
        self._run = True
        c = 0
        while self._run and (cycles is None or c < cycles):
            t = self.mac.next_tx(self.slot, time.monotonic(), self.epoch)
            time.sleep(max(0.0, t - time.monotonic()))
            self.report_once(ts_us=int(time.monotonic() * 1e6) & 0xFFFFFF)
            c += 1
            time.sleep(max(0.0, self.cadence - (time.monotonic() - t)))

    def start(self, cycles=None):
        self._th = threading.Thread(target=self.run, args=(cycles,),
                                    name=f"sense-node-{self.node_id}", daemon=True)
        self._th.start()
        return self

    def stop(self):
        self._run = False
        if self._th:
            self._th.join(timeout=2.0)
