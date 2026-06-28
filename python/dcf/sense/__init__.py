# SPDX-License-Identifier: LGPL-3.0-only
# Copyright (c) 2026 DeMoD LLC.
"""DCF-Sense — a configurable greenhouse sensor-telemetry layer over the 17-byte
DeModFrame (carried by HydraModem or any DCF transport).

  schema  — compact sensor-reading encoding (one reading = one bare DeModFrame)
  mac     — configurable media access (tdma / dedicated; fdma/csma later)
  config  — the unified node+gateway config
  node    — sensor-node agent (read -> encode -> transmit in slot)
  gateway — RX -> decode -> egress (callback / CSV)

A transport/adapter over the wire quantum: the 246-vector certificate is untouched.
See Documentation/DCF_SENSE_SPEC.md.
"""
