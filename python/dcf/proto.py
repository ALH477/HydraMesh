# SPDX-License-Identifier: LGPL-3.0-only
"""Binary ProtoMessage envelope — the UDP transport shared by the Go/C/Rust nodes.

Byte-identical to ``go/node/proto.go`` and ``rust/src/lib.rs`` ``ProtoMessage``:
a 17-byte big-endian header followed by the payload::

    [0]      msg_type     u8
    [1:5]    sequence     u32
    [5:13]   timestamp    u64   (micros since the Unix epoch)
    [13:17]  payload_len  u32
    [17:...] payload      payload_len bytes

This is the envelope; adapters (DCF-Game/Audio/Text and the DCF-Mesh REPORT/ROLE
control) ride inside ``payload``. Stdlib only — no gRPC, no third-party deps.
"""

import struct
import time

# Message-type registry — mirrors rust/src/lib.rs `mod msg_type` and go/node/proto.go.
MSG_POSITION = 1
MSG_AUDIO = 2
MSG_GAME_EVENT = 3
MSG_STATE_SYNC = 4
MSG_RELIABLE = 5
MSG_ACK = 6
MSG_PING = 7
MSG_PONG = 8
MSG_GAME_DCF = 9
MSG_TEXT_DCF = 10
MSG_MESH = 11  # DCF-Mesh REPORT/ROLE control for the self-healing runtime

HEADER_LEN = 1 + 4 + 8 + 4  # 17
_HEADER = struct.Struct(">BIQI")  # type, sequence, timestamp, payload_len


def now_micros():
    """Microseconds since the Unix epoch (matches the other SDKs' timestamp)."""
    return int(time.time() * 1_000_000)


class ProtoMessage:
    """The binary UDP transport envelope."""

    __slots__ = ("msg_type", "sequence", "timestamp", "payload")

    def __init__(self, msg_type, sequence, payload=b"", timestamp=None):
        self.msg_type = msg_type
        self.sequence = sequence & 0xFFFFFFFF
        self.timestamp = now_micros() if timestamp is None else timestamp
        self.payload = bytes(payload)

    def serialize(self):
        """Encode to bytes (17-byte big-endian header + payload)."""
        return _HEADER.pack(self.msg_type, self.sequence, self.timestamp,
                            len(self.payload)) + self.payload

    @classmethod
    def deserialize(cls, data):
        """Parse bytes into a ProtoMessage, or raise ValueError on bad input.

        Guards a short header and a payload-length field that overruns the buffer
        (mirrors the Go/Rust deserialize guards) — so a wiretap that captures
        non-DCF (e.g. encrypted) bytes simply fails here.
        """
        if len(data) < HEADER_LEN:
            raise ValueError("message shorter than 17-byte header")
        msg_type, sequence, timestamp, payload_len = _HEADER.unpack(data[:HEADER_LEN])
        if len(data) < HEADER_LEN + payload_len:
            raise ValueError("payload length exceeds message size")
        payload = data[HEADER_LEN:HEADER_LEN + payload_len]
        return cls(msg_type, sequence, payload, timestamp=timestamp)

    def __repr__(self):
        return (f"ProtoMessage(type={self.msg_type}, seq={self.sequence}, "
                f"ts={self.timestamp}, payload_len={len(self.payload)})")
