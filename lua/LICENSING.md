# Licensing — DCF-Audio Lua framework

**SPDX-License-Identifier: `LGPL-3.0-only`**
**Copyright © 2026 DeMoD LLC.**

This Lua DCF-Audio framework (`dcf_audio.lua`, `dcf_jam.lua`, `selftest.lua`, and the
sibling `GUI/audiolab.lua`) is **dual-licensed**:

1. **Open source — GNU LGPL-3.0-only.** You may use, study, modify, and redistribute
   it under the terms of the GNU Lesser General Public License, version 3.0 (see the
   repository root `LICENSE`). This framework is intentionally open and
   reverse-engineerable; that is the point.

2. **Commercial license — available from DeMoD LLC on request.** Because DeMoD LLC is
   the sole copyright holder, it can also license this framework under separate
   commercial terms for organizations that cannot meet the LGPL's obligations (for
   example, certain static-linking or redistribution scenarios). Contact DeMoD LLC to
   arrange a commercial license.

You may choose **either** license. Choosing LGPL-3.0 imposes no obligation to obtain a
commercial license.

## Scope and boundaries

- This framework is a clean, self-contained port of the **certified** DCF-Audio wire
  layer (see `Documentation/DCF_AUDIO_SPEC.md`). It depends on nothing proprietary.
- The bundled phase-mod codec spec `codec/faust/dcf_pm_codec.dsp` is likewise LGPL-3.0.
- It is **kept separate** from any source-available-but-proprietary code (e.g. the
  TERMINUS PolyForm Shield shell). Do not copy LGPL code into Shield/proprietary files
  or vice versa; depend on this framework as a discrete module instead.

## Third-party

The CRC-16/CCITT and DeModFrame layout are an open protocol specification, not
third-party code. No third-party code is vendored into this framework.
