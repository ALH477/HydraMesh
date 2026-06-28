# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF-Sense reading schema — compact greenhouse sensor data over the 17-byte DeModFrame.

The efficient default: **one reading = one bare DeModFrame** (zero adapter overhead).
The frame's `src_id` is the node id, `dst` is the gateway channel, and the 4-byte
payload is one reading:

    [ sensor_type : u8 | value : i16 big-endian (scaled) | flags : u8 ]

A value is transmitted as a scaled signed 16-bit integer; each sensor type has a fixed
scale, so the physical value is `raw_i16 / SCALE[type]`. Out-of-range values are clamped
and the CLAMPED flag is set. (Multi-sensor atomic bundles via DCF-Game EVENT and paired
SuperPack sends are efficiency options layered on top later; see DCF_SENSE_SPEC.md.)

This is an adapter *over* the wire quantum — it never changes the frame format, so the
246-vector certificate is untouched.
"""
import os
import struct
import sys

for _mcp in (os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "MCP"),
             os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "MCP")):
    if os.path.isdir(_mcp):
        sys.path.insert(0, _mcp)
import wirelab_core as wire  # noqa: E402

FDATA = 0   # DeModFrame type DATA

# Sensor registry: id -> (name, unit, scale).  physical = raw_i16 / scale.
SENSORS = {
    0: ("temp",          "degC",  100),   # 0.01 degC resolution
    1: ("humidity",      "%RH",   100),
    2: ("soil_moisture", "%",     100),
    3: ("co2",           "ppm",     1),    # 0..32767 ppm
    4: ("light_par",     "umol",    1),    # PAR umol/m^2/s
    5: ("ph",            "pH",    100),
    6: ("ec",            "mS/cm", 100),
    7: ("pressure",      "hPa",    10),
    8: ("battery",       "V",     100),
    9: ("vpd",           "kPa",   100),    # vapour-pressure deficit
}
NAME_TO_ID = {v[0]: k for k, v in SENSORS.items()}

# flags bits (opaque to the PHY)
FLAG_CLAMPED = 0x01   # value was clamped to the i16 range
FLAG_ALARM   = 0x02   # node-side threshold alarm
FLAG_LOWBATT = 0x04


def scale_of(sensor_type):
    return SENSORS.get(sensor_type, (None, None, 1))[2]


def name_of(sensor_type):
    return SENSORS.get(sensor_type, (f"type{sensor_type}", "", 1))[0]


def encode_value(sensor_type, value):
    """Physical float -> (raw_i16, clamped?)."""
    raw = int(round(value * scale_of(sensor_type)))
    clamped = raw < -32768 or raw > 32767
    return max(-32768, min(32767, raw)), clamped


def decode_value(sensor_type, raw_i16):
    return raw_i16 / scale_of(sensor_type)


def encode_reading(node_id, sensor_type, value, *, dst=0xFFFF, seq=0, ts_us=0, flags=0):
    """One reading -> one 17-byte DeModFrame (src_id = node_id)."""
    raw, clamped = encode_value(sensor_type, value)
    if clamped:
        flags |= FLAG_CLAMPED
    payload = struct.pack(">BhB", sensor_type & 0xFF, raw, flags & 0xFF)
    return wire.encode(FDATA, seq & 0xFFFF, node_id & 0xFFFF, dst & 0xFFFF, payload, ts_us)


def decode_reading(frame):
    """17-byte DeModFrame -> reading dict, or None if it is not a valid DATA reading."""
    try:
        d = wire.decode(bytes(frame))
    except ValueError:
        return None
    if d["frame_type"] != FDATA:
        return None
    payload = d["payload"]
    payload = bytes.fromhex(payload) if isinstance(payload, str) else bytes(payload)
    if len(payload) != 4:
        return None
    sensor_type, raw, flags = struct.unpack(">BhB", payload)
    name, unit, _ = SENSORS.get(sensor_type, (f"type{sensor_type}", "", 1))
    return {
        "node_id": d["src"], "dst": d["dst"], "seq": d["seq"], "ts_us": d["ts_us"],
        "sensor_type": sensor_type, "sensor": name, "unit": unit,
        "value": decode_value(sensor_type, raw), "raw": raw, "flags": flags,
    }
