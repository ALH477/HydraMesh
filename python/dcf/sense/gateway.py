# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF-Sense gateway: receive DeModFrames over any DCF transport, decode DCF-Sense
readings, and forward them to an egress (callback and/or CSV). HydraModem's streaming
RX is already keyed per src_id, so per-node separation is free."""
from . import schema


class Gateway:
    def __init__(self, transport, on_reading=None, csv_path=None):
        self.transport = transport
        self.on_reading = on_reading
        self.csv_path = csv_path
        self.readings = []          # in-memory log of decoded readings
        self.frames = 0             # frames delivered by the transport
        self.decoded = 0            # frames that decoded as DCF-Sense readings
        self._csv = None

    def _on_frame(self, frame, meta=None):
        self.frames += 1
        r = schema.decode_reading(frame)
        if r is None:
            return
        self.decoded += 1
        self.readings.append(r)
        if self._csv:
            self._csv.write("%d,%d,%s,%g,%s,%d\n" % (
                r["ts_us"], r["node_id"], r["sensor"], r["value"], r["unit"], r["flags"]))
            self._csv.flush()
        if self.on_reading:
            self.on_reading(r)

    def start(self):
        if self.csv_path:
            self._csv = open(self.csv_path, "w")
            self._csv.write("ts_us,node_id,sensor,value,unit,flags\n")
        self.transport.start(self._on_frame)
        return self

    def stop(self):
        self.transport.stop()
        if self._csv:
            self._csv.close()
            self._csv = None
