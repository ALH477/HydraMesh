# SPDX-License-Identifier: LGPL-3.0-only
"""Unit tests for the Python self-healing mesh runtime + binary envelope.

The certified primitives themselves are pinned by python/MCP/gen_mesh_vectors.py;
these tests cover the *runtime* wiring (FSM window, control dispatch) and the
ProtoMessage envelope round-trip.
"""

import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))

import meshlab_core as mesh
from dcf.proto import ProtoMessage, MSG_MESH, HEADER_LEN
from dcf.mesh_runtime import MeshRuntime


def _tick(rt, peers, n, answered=False):
    for _ in range(n):
        if answered:
            for p in peers:
                rt.mark_answered(p)
        rt._health_tick(peers)


def test_fsm_window_unreachable_then_recovers():
    rt = MeshRuntime("auto", 1, "0", 50)
    peers = ["2"]
    _tick(rt, peers, 3, answered=False)          # 3 timeouts -> UNREACHABLE
    assert rt._status_of("2") == mesh.UNREACHABLE
    _tick(rt, peers, 2, answered=True)           # 2 PONGs -> recover
    assert rt._status_of("2") == mesh.HEALTHY


def test_degraded_after_single_timeout():
    rt = MeshRuntime("auto", 1, "0", 50)
    peers = ["2"]
    _tick(rt, peers, 1, answered=True)           # ok
    _tick(rt, peers, 1, answered=False)          # one timeout -> DEGRADED
    assert rt._status_of("2") == mesh.DEGRADED


def test_role_adoption_via_control():
    rt = MeshRuntime("auto", 7, "0", 50)
    payload = mesh.pack_role(7, mesh.RELAY, 3)
    rt.handle_control(payload, ("127.0.0.1", 9000))
    assert rt.role == mesh.RELAY
    assert rt.master == 3
    # A ROLE addressed to a different node is ignored.
    rt.handle_control(mesh.pack_role(99, mesh.MASTER, 99), ("127.0.0.1", 9000))
    assert rt.master == 3


def test_master_records_reports():
    rt = MeshRuntime("master", 0, "", 50)
    report = mesh.pack_report(1, [(0, mesh.HEALTHY, 12), (2, mesh.HEALTHY, 20)])
    rt.handle_control(report, ("127.0.0.1", 7001))
    assert 1 in rt._reports
    assert rt._report_addr[1] == ("127.0.0.1", 7001)


def test_protomessage_roundtrip():
    msg = ProtoMessage(MSG_MESH, 42, b"hello-wire")
    raw = msg.serialize()
    assert len(raw) == HEADER_LEN + len(b"hello-wire")
    back = ProtoMessage.deserialize(raw)
    assert back.msg_type == MSG_MESH
    assert back.sequence == 42
    assert back.timestamp == msg.timestamp
    assert back.payload == b"hello-wire"


def test_protomessage_rejects_short_and_overrun():
    import pytest
    with pytest.raises(ValueError):
        ProtoMessage.deserialize(b"\x00\x01\x02")          # < 17 bytes
    truncated = ProtoMessage(MSG_MESH, 1, b"abcdef").serialize()[:-2]
    with pytest.raises(ValueError):
        ProtoMessage.deserialize(truncated)                 # payload overruns
