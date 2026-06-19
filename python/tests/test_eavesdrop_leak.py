# SPDX-License-Identifier: LGPL-3.0-only
"""Data-leak demonstration: the encryption-free DCF wire is fully readable on-path.

DCF is plaintext by design (EAR/ITAR export compliance). This test *proves* the
consequence: a passive :class:`~dcf.wiretap.Wiretap` holding no credentials, sitting
on the path between two real mesh nodes, recovers the entire mesh from the wire —
membership + topology + RTT (REPORT), the master assignment (ROLE), and message
contents. It then shows the mitigation: wrapping each datagram with an *external*
transport encryptor (modeling WireGuard, applied outside DCF) leaves the same wiretap
with nothing to decode.

The wrapper here is a trivial XOR stand-in living only in the demo — NOT crypto, and
never in the DCF core wire path. In production the wrapper is WireGuard. See
Documentation/DCF_SECURITY_EXPOSURE.md.
"""

import os
import socket
import sys
import time

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "MCP"))

import meshlab_core as mesh
from dcf.proto import ProtoMessage, MSG_TEXT_DCF, MSG_MESH
from dcf.udp_node import DcfNode
from dcf.mesh_runtime import MeshRuntime
from dcf.wiretap import Wiretap, describe, external_wrap, external_unwrap

SECRET = "rendezvous@145.500MHz: extraction at 0300"


def _find(decoded, msg_type, pred):
    for m in decoded:
        if m.msg_type == msg_type and pred(m):
            return m
    return None


def test_passive_wiretap_reconstructs_plaintext_mesh():
    # A master (node 0) and an auto node (node 1). The auto node reaches the master
    # *through* a wiretap on :7100 — a malicious on-path hop.
    master = DcfNode("127.0.0.1", 7000, node_id="0")
    master.mesh = MeshRuntime("master", 0, "", group_thr=50)
    auto = DcfNode("127.0.0.1", 7001, node_id="1")
    auto.mesh = MeshRuntime("auto", 1, master_peer="0", group_thr=50)

    tap = Wiretap(("127.0.0.1", 7100), ("127.0.0.1", 7000), label="EVE")
    auto.add_peer("0", "127.0.0.1", 7100)    # auto -> master via the tap
    master.add_peer("1", "127.0.0.1", 7001)  # master -> auto direct

    tap.start()
    master.start()
    auto.start()
    try:
        # A "secret" application message crossing the same plaintext wire.
        for _ in range(3):
            auto.send_to(MSG_TEXT_DCF, SECRET.encode(), "127.0.0.1", 7100)
            time.sleep(0.3)

        # Wait (<= ~8s) until the tap has captured a REPORT, a ROLE, and the text.
        report = role = text = None
        deadline = time.time() + 8
        while time.time() < deadline:
            decoded = tap.decoded()
            report = _find(decoded, MSG_MESH,
                           lambda m: mesh.mesh_msg_type(m.payload) == mesh.MESH_REPORT)
            role = _find(decoded, MSG_MESH,
                         lambda m: mesh.mesh_msg_type(m.payload) == mesh.MESH_ROLE)
            text = _find(decoded, MSG_TEXT_DCF, lambda m: SECRET.encode() in m.payload)
            if report and role and text:
                break
            time.sleep(0.3)
    finally:
        auto.stop()
        master.stop()
        tap.stop()

    # --- THE LEAK: EVE, with zero credentials, reconstructs everything. ---
    assert text is not None, "eavesdropper did not capture the application message"
    assert SECRET in text.payload.decode(), "secret message recovered verbatim"

    assert report is not None, "eavesdropper did not capture a REPORT"
    nid, peers = mesh.unpack_report(report.payload)
    assert nid == 1                                   # learned the reporting node id
    assert any(p[0] == 0 for p in peers)              # learned its peer (the master)
    # describe() is what the live CLI prints — confirm it spells out the topology.
    assert "REPORT" in describe(report)

    assert role is not None, "eavesdropper did not capture a ROLE"
    _rnid, _role, master_id = mesh.unpack_role(role.payload)
    assert master_id == 0                             # learned who the master is


def test_external_wrap_defeats_the_wiretap():
    # The exact bytes a node would put on the wire for a REPORT.
    report = ProtoMessage(MSG_MESH, 1,
                          mesh.pack_report(1, [(0, mesh.HEALTHY, 12)])).serialize()

    # Plaintext on the wire -> the wiretap decodes it (the leak).
    assert ProtoMessage.deserialize(report).msg_type == MSG_MESH

    # Wrapped by an external transport encryptor (WireGuard stand-in) -> on the wire
    # it is no longer a DCF ProtoMessage, so the wiretap recovers nothing.
    on_wire = external_wrap(report)
    assert on_wire != report
    try:
        ProtoMessage.deserialize(on_wire)
        decoded_ok = True
    except ValueError:
        decoded_ok = False
    assert not decoded_ok, "external wrapper must make the datagram undecodable to a tap"

    # Drive it through an actual Wiretap to confirm it logs/recovers nothing usable.
    sink = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sink.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sink.bind(("127.0.0.1", 7202))
    tap = Wiretap(("127.0.0.1", 7201), ("127.0.0.1", 7202), label="EVE")
    tap.start()
    try:
        tx = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        for _ in range(3):
            tx.sendto(on_wire, ("127.0.0.1", 7201))
            time.sleep(0.1)
        time.sleep(0.5)
    finally:
        tap.stop()
        sink.close()
    assert len(tap.captures) >= 1                      # bytes did cross the wire
    assert tap.decoded() == []                         # ...but none decode as DCF

    # The legitimate endpoint, holding the key, still recovers the frame.
    assert external_unwrap(on_wire) == report
    assert ProtoMessage.deserialize(external_unwrap(on_wire)).msg_type == MSG_MESH
