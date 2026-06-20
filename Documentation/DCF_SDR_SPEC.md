# DCF-SDR — DeModFrames over radio (IQ modem + FM/AM/FSK/PSK)

DCF is medium-agnostic by construction. DCF-SDR makes that real over RF: a
DeModFrame is FEC-coded and rendered to **complex IQ baseband** for a SoapySDR
device (HackRF / PlutoSDR / LimeSDR / rtl-sdr / USRP) or a hardware-agnostic
`.cf32` file (GNU Radio, `rtl_sdr`, `hackrf_transfer`).

```
frame(17B) → RS-FEC (certified) → byte↔symbol map (certified) → complex IQ → radio
                                                                       ↓
frame ← rs_decode ← demod ← IQ ← radio
```

Two layers, like the rest of DCF: the **byte↔symbol map** (`codec/demod_modulation.h`)
and the **RS-FEC** (`DCF_FEC_SPEC.md`) are byte-certified; the **IQ waveform** here
(`python/modem/iq.py`) is analog and loopback-tested. The FEC is what makes it
*legitimately useful* over a lossy channel — AWGN/burst errors are corrected, not
just detected.

## Modulations

| `--mod` | What | Demod | Notes |
|---|---|---|---|
| `fsk` / `gfsk` | 2-FSK / Gaussian FSK (CPFSK) | FM discriminator | the ISM/ham data workhorse |
| `psk` / `qpsk` | BPSK/QPSK (certified constellation) | coherent (nearest) | spectrally efficient |
| `qam` | 16-QAM (certified constellation) | nearest-point | most bits/Hz, needs SNR |
| `ook` / `ask` / `am` | on-off / amplitude | envelope | simplest, cheap radios |
| `afsk-fm` | Bell-202 AFSK NBFM'd onto the carrier | FM discriminator + non-coherent FSK | **interoperates with real FM radios / repeaters** (APRS-style) |

## CLI (`dcf-sdr`, in `nix develop .#sdr`)

```sh
# transmit to a file (hardware-agnostic) or a SoapySDR device
dcf-sdr tx --text "hello" --mod gfsk --iq out.cf32
dcf-sdr tx --dcf capture.dcf --mod qpsk --soapy driver=hackrf --freq 433.9M --rate 2M
# pipe the file to any SDR transmitter:
hackrf_transfer -t out.cf32 -f 433900000 -s 2000000

# receive from a file or device
dcf-sdr rx --iq out.cf32 --mod gfsk
dcf-sdr rx --soapy driver=rtlsdr --freq 433.9M --rate 2M --mod gfsk --secs 3 --out got.dcf
```

`.cf32` is interleaved float32 I,Q — the lingua franca of `rtl_sdr`,
`hackrf_transfer`, and GNU Radio. SoapySDR is a **soft dependency**: files and the
software loopback work without hardware or the python bindings.

## Receiver synchronization

`rx` / `iq_to_frame(sync=True)` recovers a real off-air burst, not just clean
loopback:
- **Energy onset** locates the burst after the leading delay.
- **Carrier-frequency offset (CFO)** is estimated from the preamble (constant-symbol
  autocorrelation for PSK/QAM; FM-discriminator DC removal for FSK/AFSK) and removed.
- **Symbol timing / frame start** by cross-correlating the known preamble+sync.
- **Static phase** for coherent PSK/QAM from the known preamble.

Tested under CFO + timing delay + AWGN: every modulation recovers ~20/20 where naive
demod gets 0/20 (`python/tests/test_iq_loopback.py::test_offair_sync`).

## Status & limits

- **Certified-clean**: the `.cf32`/software-loopback path and the round-trip
  `tx --dcf … | rx --out …` are byte-exact; off-air-style captures decode via the
  synchronizer above. The FEC + byte↔symbol map are the certified, hardware-
  independent contract; the waveform/sync are loopback-tested.
- Synchronization is single-burst and assumes a moderate CFO (≲1% of the sample
  rate) and integer-sample timing; very low SNR, large CFO, fading, or
  back-to-back bursts at speed want a tracking loop (future).
- A **Faust** IQ modulator (`codec/faust/dcf_rf_modulator.dsp` → committed
  `dcf_rf_modulator.gen.c`, CI diff-checked like the PM synth) is the normative I/Q
  waveform spec (CPFSK / OOK / QPSK / 16-QAM, 2-output I,Q); the numpy renderer in
  `python/modem/iq.py` is the functional reference + demodulator.

## Legal & security

Receiving is generally license-free; **transmitting needs a license and/or an ISM
band** (e.g. 433.9 MHz / 915 MHz) — you are responsible for compliance. The DCF wire
is plaintext by design and **RF has no WireGuard** — treat an RF link as a public
broadcast (membership, contents, topology are all readable). See
`DCF_SECURITY_EXPOSURE.md`.
