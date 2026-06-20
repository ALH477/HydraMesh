# SPDX-License-Identifier: LGPL-3.0-only
"""DCF reference codecs / "labs" — the certified Python implementations of the wire
quantum and its adapters (wirelab, modulationlab, feclab, audiolab, gamelab, meshlab).

Modules here are written to be runnable as flat scripts (``python3 python/MCP/foo.py``)
*and* importable when the tree ships inside the ``demod-dcf`` distribution as
``dcf.MCP``. The flat ``import wirelab_core`` style inside these modules keeps working
in both layouts because each module's own directory is placed on ``sys.path`` first.
"""
