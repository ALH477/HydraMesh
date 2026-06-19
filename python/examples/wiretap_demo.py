#!/usr/bin/env python3
# SPDX-License-Identifier: LGPL-3.0-only
"""Live demo: a passive wiretap reconstructs an encryption-free DCF mesh.

Spins up a 2-node mesh (a master + an auto node) whose peer traffic is routed through
a :class:`~dcf.wiretap.Wiretap`. The wiretap holds no credentials and participates in
nothing, yet it decodes and prints the full plaintext stream: PING/PONG liveness, the
auto node's REPORT (its peer view + RTT), and the master's ROLE assignment. That is
the leak the encryption-free wire exposes to any on-path party.

Run::

    python3 python/examples/wiretap_demo.py

Then read ``Documentation/DCF_SECURITY_EXPOSURE.md``: the fix is to run DCF inside a
WireGuard tunnel (or the operator's own export-compliant crypto), beneath this socket.
"""

import logging
import os
import sys
import time

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from dcf.udp_node import DcfNode
from dcf.mesh_runtime import MeshRuntime
from dcf.wiretap import Wiretap


def main():
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(name)s %(message)s")

    # Ports: master=7000, auto=7001, and a wiretap on 7100 that forwards to the master.
    master = DcfNode("127.0.0.1", 7000, node_id="0")
    master.mesh = MeshRuntime("master", 0, "", group_thr=50)

    auto = DcfNode("127.0.0.1", 7001, node_id="1")
    auto.mesh = MeshRuntime("auto", 1, master_peer="0", group_thr=50)

    # The auto node reaches the master *through* the wiretap (a malicious on-path hop).
    tap = Wiretap(("127.0.0.1", 7100), ("127.0.0.1", 7000), label="EVE")
    auto.add_peer("0", "127.0.0.1", 7100)   # auto -> master via the tap
    master.add_peer("1", "127.0.0.1", 7001)  # master -> auto direct (replies still seen)

    tap.start()
    master.start()
    auto.start()
    logging.info("running 2-node mesh through wiretap EVE for ~6s ...")
    try:
        time.sleep(6)
    finally:
        auto.stop()
        master.stop()
        tap.stop()

    decoded = tap.decoded()
    logging.info("EVE captured %d datagrams, %d decoded as DCF (zero credentials)",
                 len(tap.captures), len(decoded))


if __name__ == "__main__":
    main()
