// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//
//! The datagram gate. Every datagram the bridge relays, browser → mesh and
//! mesh → browser, must be one of the two things the bare DCF dialect carries
//! (`Documentation/DCF_MEDIUM_SPEC.md`, `udp_bare`):
//!
//! - a 17-byte `DeModFrame`: sync `0xD3`, version nibble 1, and a valid
//!   CRC-16/CCITT-FALSE over bytes 0..14;
//! - a 32-byte SuperPack: sync `0xD3`, sflags `0x15`, a valid joint CRC over
//!   bytes 0..29, and both inner cores at version 1.
//!
//! Anything else is dropped. The type nibble is not gated.
//!
//! The decision is not made in Rust. It is Exsecutor's `examples/custos`,
//! compiled as one unit with the `DeModFrame` codec that passes Exsecutor's
//! §14 entry 23 (the 246-vector certificate), emitted as C
//! (`custos/custos.gen.c`, see `custos/PROVENANCE.md`), and linked here. This
//! wrapper only copies the bytes into a 32-byte window and names the verdict.
//! `tests/certify_gate.rs` holds it to Punctim's vectors and to the Rust
//! reference codec.

/// Why a datagram was refused, or that it was admitted. The codes are
/// `custos.exsc`'s; only admitted-or-not is certified.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Verdict {
    Admitted,
    BadSync,
    BadVersion,
    BadCrc,
    /// 32 bytes whose sflags type nibble is not SUPER (5).
    NotSuperPack,
    /// A SuperPack whose joint CRC holds but one core is not version 1.
    BadCoreVersion,
    /// Neither 17 nor 32 bytes.
    BadLength,
    /// A code the gate does not document. Refused: fail closed.
    Unknown(u64),
}

impl Verdict {
    pub fn admitted(self) -> bool {
        self == Verdict::Admitted
    }
}

extern "C" {
    // custos.exsc: `publica functio admitte(d: acies<u8, 32>, n: mensura) -> u8`,
    // which the C backend emits as `uint64_t exs_admitte(unsigned char *, uint64_t)`.
    fn exs_admitte(d: *mut u8, n: u64) -> u64;
}

/// The gate's verdict on one datagram.
pub fn admit(datagram: &[u8]) -> Verdict {
    // The gate reads `d[0..n)` only when n is 17 or 32, and decides every
    // other length without reading at all. So the window holds at most the
    // first 32 bytes and the true length goes in unchanged: the length check
    // is the gate's too, not this wrapper's.
    let mut window = [0u8; 32];
    let k = datagram.len().min(window.len());
    window[..k].copy_from_slice(&datagram[..k]);
    // SAFETY: `window` is a local 32-byte array, exactly the `acies<u8, 32>`
    // the function is declared over; the unit reads no byte past it. The
    // callee has no other state (pure: no `poscit`, no globals), so it is
    // safe to call from any thread. A trap inside it calls
    // `exsrt_abortus`, which stops the process (custos/shim.c).
    let code = unsafe { exs_admitte(window.as_mut_ptr(), datagram.len() as u64) };
    match code {
        0 => Verdict::Admitted,
        1 => Verdict::BadSync,
        2 => Verdict::BadVersion,
        3 => Verdict::BadCrc,
        4 => Verdict::NotSuperPack,
        5 => Verdict::BadCoreVersion,
        6 => Verdict::BadLength,
        other => Verdict::Unknown(other),
    }
}
