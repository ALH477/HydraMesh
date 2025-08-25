# God said to Moses, “I AM WHO I AM. This is what you are to say to the Israelites: ‘I AM has sent me to you.’” (Exodus 3:14)
# Copyright (C) 2025 DeMoD LLC
# This file is part of DeMoD Communications Framework.
# Licensed under GPL-3.0 (see LICENSE in repo root).
import time
from . import messages_pb2

def build_message(**kwargs):
    msg = messages_pb2.DCFMessage()
    for k, v in kwargs.items():
        setattr(msg, k, v)
    msg.timestamp = int(time.time() * 1000)
    return msg

def parse_message(serialized):
    msg = messages_pb2.DCFMessage()
    msg.ParseFromString(serialized)
    return msg
