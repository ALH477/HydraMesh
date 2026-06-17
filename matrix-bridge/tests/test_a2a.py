"""Agent-to-agent tests — two DcfTextNodes conversing over the DeModFrame mesh.

Pins the behaviour the agent-to-agent setup relies on: two endpoints peered on a
shared channel exchange multi-fragment messages each way, with distinct src ids;
a blocking receive returns within its timeout; and a node on a different channel
does not pick up the traffic.  Pure stdlib — runs under pytest or directly.
"""
import os
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, ".."))
sys.path.insert(0, os.path.join(HERE, "..", "..", "python", "MCP"))

from dcf_node import DcfTextNode


def _pair(channel="duet", superpack=True):
    a = DcfTextNode(node_id=0x00A1, port=0, channel=channel, use_superpack=superpack)
    b = DcfTextNode(node_id=0x00B2, port=0, channel=channel, use_superpack=superpack)
    a.add_peer("b", "127.0.0.1", b.port)
    b.add_peer("a", "127.0.0.1", a.port)
    a.start()
    b.start()
    return a, b


def test_bidirectional_multifragment_exchange():
    """Each side sends a multi-fragment message; both reassemble with right src."""
    a, b = _pair()
    try:
        msg_ab = "Ada -> Boole, fragment me \U0001f680 " * 5     # > 4 bytes/frame
        msg_ba = "Boole -> Ada, and back again ❤️ " * 5
        a.send_text(msg_ab, to="b")
        got = b.poll(timeout=3.0)
        assert got is not None, "B never received A's message"
        assert got[3] == msg_ab and got[1] == 0x00A1

        b.send_text(msg_ba, to="a")
        got = a.poll(timeout=3.0)
        assert got is not None, "A never received B's reply"
        assert got[3] == msg_ba and got[1] == 0x00B2
    finally:
        a.stop()
        b.stop()


def test_blocking_recv_returns_within_timeout():
    """A blocking poll returns promptly once the peer sends (mesh_recv semantics)."""
    a, b = _pair()
    try:
        start = time.time()
        a.send_text("quick ping", to="b")
        got = b.poll(timeout=5.0)
        assert got is not None and got[3] == "quick ping"
        assert time.time() - start < 5.0, "blocking recv did not return before timeout"
    finally:
        a.stop()
        b.stop()


def test_blocking_recv_times_out_when_silent():
    """No traffic -> blocking poll returns None at the deadline, not forever."""
    a, b = _pair()
    try:
        assert b.poll(timeout=0.3) is None
    finally:
        a.stop()
        b.stop()


def test_wrong_channel_is_not_received():
    """A node on a different channel must not pick up the traffic."""
    a = DcfTextNode(node_id=0x00A1, port=0, channel="duet", use_superpack=True)
    eve = DcfTextNode(node_id=0x00EE, port=0, channel="other", use_superpack=True)
    a.add_peer("eve", "127.0.0.1", eve.port)   # deliberately mis-routed to a foreign channel
    eve.start()
    try:
        a.send_text("not for you", to="eve")
        assert eve.poll(timeout=0.5) is None, "eve received traffic on the wrong channel"
    finally:
        a.stop()
        eve.stop()


if __name__ == "__main__":
    test_bidirectional_multifragment_exchange()
    test_blocking_recv_returns_within_timeout()
    test_blocking_recv_times_out_when_silent()
    test_wrong_channel_is_not_received()
    print("test_a2a: CERTIFIED (bidirectional, blocking recv, channel isolation)")
