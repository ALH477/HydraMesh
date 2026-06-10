# Experimental / Quarantined Transports

These transports are **not compiled** and **not shipped**. Each has significant
issues that prevent compilation or correct operation against the current SDK API.

Move a transport back to `../` only after it has been ported to the current
`DCFTransportV1` or `ITransport` (v2) interface and passes review.

| File | Reason for quarantine |
|---|---|
| `bluetooth_transport.c` | Unbounded `measure_rtt`→`send` recursion; implicit declarations; nonexistent `dcf_parse_message`; hardcoded 10-peer array with no bounds check |
| `unified_dual_transport.c` | Sends a struct pointer as payload via a shadowed `data`; `send` symbol clashes with POSIX `send(2)`; references extinct `DCFConfig` shape |
| `quic_transport.c` | Misuses MsQuic API — `MsQuicOpen2` returns an API table, not a connection |
| `irc_transport.c` | Base64 encoder mishandles inputs not a multiple of 3; reads past buffer |
| `zigbee_transport.c` | References a fictional API (`zigbee_init`, etc.) that has no implementation |