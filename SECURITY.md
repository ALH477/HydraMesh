# Security Policy

## Reporting a vulnerability

Please report security issues **privately** to **alh477@demod.ltd** rather than opening a public
issue. Include a description, affected component/language binding, and reproduction steps if you
have them. We aim to acknowledge reports within a few days and will coordinate a fix and
disclosure timeline with you.

## Scope and threat model

DCF is **encryption-free by design** for export-control compliance (EAR/ITAR) — see
[`Documentation/Specs/export_compliance.markdown`](Documentation/Specs/export_compliance.markdown).
The 17-byte `DeModFrame` wire protocol provides **error detection (CRC-16), not authentication or
confidentiality**. Confidentiality and access control are the responsibility of the network
underlay (run DCF inside a VPN such as WireGuard/Tailscale); do not expose the plaintext mesh to
untrusted networks.

Given that, the most valuable reports are:

- **Memory-safety defects** in the codec or SDKs (use-after-free, out-of-bounds, uninitialized
  reads) — especially in the C SDK and the FFI boundaries.
- **Wire-codec correctness** issues where an implementation diverges from
  `Documentation/golden_vectors.json` (a divergence is both a bug and a potential interop hazard).
- **Denial-of-service** reachable from malformed frames.

Out of scope: the absence of encryption in the core wire path (this is intentional and
documented), and confidentiality of traffic on an untrusted network without a VPN underlay.

## Supported versions

The project is pre-1.0 and under active development; security fixes land on the default branch.
Once tagged releases exist, this section will list the supported version range.
