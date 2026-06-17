"""Tests for the non-MCP CLI bridge (matrix-bridge/a2a_cli.py).

Stdlib loopback over UDP (mirrors tests/test_a2a.py): a `send` reaches a real
DcfTextNode with the right src; a one-shot `recv` returns 0 on a message and 1 on a
silent timeout.  Run: `python3 matrix-bridge/tests/test_a2a_cli.py`.
"""
import os
import socket
import sys
import threading
import time

HERE = os.path.dirname(os.path.abspath(__file__))
BRIDGE = os.path.dirname(HERE)
sys.path.insert(0, BRIDGE)
sys.path.insert(0, os.path.join(BRIDGE, "..", "python", "MCP"))
import a2a_cli
from dcf_node import DcfTextNode


def _free_udp_port():
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.bind(("127.0.0.1", 0))
    port = s.getsockname()[1]
    s.close()
    return port


def test_send_reaches_a_node():
    rx = DcfTextNode(node_id=0x00B2, port=0, channel="duet", use_superpack=True)
    rx.start()
    try:
        rc = a2a_cli.send("hello from the cli \U0001f680", channel="duet",
                          peers=f"127.0.0.1:{rx.port}", node_id=0x00A1, json_out=True)
        assert rc == 0, "send should succeed"
        ev = rx.poll(timeout=3.0)
    finally:
        rx.stop()
    assert ev is not None, "node received nothing"
    assert ev[3] == "hello from the cli \U0001f680", "payload mismatch"
    assert ev[1] == 0x00A1, "src id mismatch"
    print("  PASS  `a2a send` reassembles at a real node with the right src")


def test_recv_oneshot_returns_zero_on_message():
    port = _free_udp_port()
    result = {}

    def run_recv():
        result["rc"] = a2a_cli.recv(channel="duet", port=port, bind="127.0.0.1",
                                    timeout=4.0, count=1, json_out=True)

    t = threading.Thread(target=run_recv)
    t.start()
    time.sleep(0.3)                      # let recv bind before we send
    tx = DcfTextNode(node_id=0x00A1, port=0, channel="duet", use_superpack=True)
    tx.add_peer("r", "127.0.0.1", port)
    tx.start()
    tx.send_text("ping the cli receiver", flags=0x01)
    t.join(timeout=6.0)
    tx.stop()
    assert result.get("rc") == 0, "one-shot recv should exit 0 after a message"
    print("  PASS  `a2a recv` one-shot returns 0 on a received message")


def test_recv_times_out_nonzero_when_silent():
    port = _free_udp_port()
    rc = a2a_cli.recv(channel="duet", port=port, bind="127.0.0.1", timeout=0.5, count=1)
    assert rc == 1, "silent recv should time out with a nonzero exit"
    print("  PASS  `a2a recv` times out nonzero when silent")


def test_wrong_channel_is_not_received():
    port = _free_udp_port()
    result = {}

    def run_recv():
        result["rc"] = a2a_cli.recv(channel="duet", port=port, bind="127.0.0.1",
                                    timeout=1.0, count=1)

    t = threading.Thread(target=run_recv)
    t.start()
    time.sleep(0.3)
    tx = DcfTextNode(node_id=0x00A1, port=0, channel="other-channel", use_superpack=True)
    tx.add_peer("r", "127.0.0.1", port)
    tx.start()
    tx.send_text("should be ignored", flags=0x01)
    t.join(timeout=3.0)
    tx.stop()
    assert result.get("rc") == 1, "a message on a different channel must not be received"
    print("  PASS  channel isolation holds for `a2a recv`")


def main():
    test_send_reaches_a_node()
    test_recv_oneshot_returns_zero_on_message()
    test_recv_times_out_nonzero_when_silent()
    test_wrong_channel_is_not_received()
    print("test_a2a_cli: CERTIFIED")


if __name__ == "__main__":
    main()
