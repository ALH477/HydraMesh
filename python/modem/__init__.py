# SPDX-License-Identifier: LGPL-3.0-only
"""DCF software-defined-radio modem: carry a 17-byte DeModFrame over complex baseband
(GFSK/QPSK/16-QAM/OOK·AM/AFSK-over-FM) with Reed-Solomon FEC, to a SoapySDR device or
a hardware-agnostic ``.cf32`` file. The ``dcf-sdr`` console command is ``sdr:main``.
"""
