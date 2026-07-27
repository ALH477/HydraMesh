// SPDX-License-Identifier: LGPL-3.0-only
//! DCF-Pipe Multi-Control — parallel Pipe control bus over one quantum.
//!
//! Packs up to three steady-state DCF-Pipe commands into a single 4-byte
//! DeModFrame payload, so a node can steer up to three concurrent lossless
//! pipes per quantum on bandwidth-scarce links.
//!
//! Byte-aligned layout (exactly 4 bytes, big-endian, MSB-first):
//! ```text
//! Byte 0: [magic:2 | count:2 | flags:4]  = 0xC0 | (count<<4) | flags
//! Byte 1: cmd[0] = (local_idx<<6) | (opcode<<3) | (param_lsb<<2)  [bits 7-2, 1-0 pad]
//! Byte 2: cmd[1]
//! Byte 3: cmd[2]
//! count ∈ 1..3; higher cmd bytes zero when count<3; opcode 111 rejected.
//! ```
//!
//! Byte-certified across C/Rust/Python by Documentation/pipemulti_vectors.json.
//! The 246-vector wire certificate and pipe_vectors.json are untouched.

// ── Constants ────────────────────────────────────────────────────────────────
pub const MC_VERSION: u8 = 1;
pub const MC_PAYLOAD_LEN: usize = 4;
pub const MC_MAGIC_MASK: u8 = 0xC0;
pub const MC_MAGIC: u8 = 0xC0;
pub const MC_MAX_COUNT: usize = 3;

// ── Opcodes ──────────────────────────────────────────────────────────────────
pub const OP_NOP: u8 = 0;
pub const OP_CREDIT_DELTA: u8 = 1;
pub const OP_ACK_CUMUL: u8 = 2;
pub const OP_ACK_SELECTIVE: u8 = 3;
pub const OP_NACK_ONE: u8 = 4;
pub const OP_DONE_HINT: u8 = 5;
pub const OP_ABORT_HINT: u8 = 6;
pub const OP_RESERVED: u8 = 7;

// ── Command slot ────────────────────────────────────────────────────────────
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cmd {
    pub local_idx: u8,
    pub opcode: u8,
    pub param_lsb: u8,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum McError {
    BadMagic,
    BadCount,
    ReservedFlags,
    ReservedOpcode,
    BadPadBits,
    UnusedSlotNonzero,
    TooManyCommands,
    BadLocalIdx,
    BadParam,
}

impl Cmd {
    pub fn new(local_idx: u8, opcode: u8, param_lsb: u8) -> Result<Self, McError> {
        if local_idx > 3 { return Err(McError::BadLocalIdx); }
        if opcode > 6 { return Err(McError::ReservedOpcode); }
        if param_lsb > 1 { return Err(McError::BadParam); }
        Ok(Cmd { local_idx, opcode, param_lsb })
    }

    fn encode(&self) -> u8 {
        ((self.local_idx & 0x03) << 6) | ((self.opcode & 0x07) << 3) | ((self.param_lsb & 0x01) << 2)
    }

    fn decode(b: u8) -> Result<Self, McError> {
        let local_idx = (b >> 6) & 0x03;
        let opcode = (b >> 3) & 0x07;
        let param_lsb = (b >> 2) & 0x01;
        if opcode == OP_RESERVED { return Err(McError::ReservedOpcode); }
        if b & 0x03 != 0 { return Err(McError::BadPadBits); }
        Ok(Cmd { local_idx, opcode, param_lsb })
    }
}

// ── Discriminators ──────────────────────────────────────────────────────────
pub fn is_multicontrol(buf: &[u8]) -> bool {
    !buf.is_empty() && (buf[0] & MC_MAGIC_MASK) == MC_MAGIC
}

pub fn is_classic_pipe(buf: &[u8]) -> bool {
    !buf.is_empty() && buf[0] <= 5
}

// ── Pack / unpack ───────────────────────────────────────────────────────────
pub fn pack_multicontrol(cmds: &[Cmd], flags: u8) -> Result<[u8; MC_PAYLOAD_LEN], McError> {
    let n = cmds.len();
    if n == 0 || n > MC_MAX_COUNT { return Err(McError::BadCount); }
    if flags != 0 { return Err(McError::ReservedFlags); }
    let mut out = [0u8; MC_PAYLOAD_LEN];
    out[0] = MC_MAGIC | ((n as u8 & 0x03) << 4) | (flags & 0x0F);
    for (i, c) in cmds.iter().enumerate() {
        if c.local_idx > 3 { return Err(McError::BadLocalIdx); }
        if c.opcode > 6 { return Err(McError::ReservedOpcode); }
        if c.param_lsb > 1 { return Err(McError::BadParam); }
        out[1 + i] = c.encode();
    }
    Ok(out)
}

pub struct Unpacked {
    pub count: u8,
    pub flags: u8,
    pub cmds: Vec<Cmd>,
}

pub fn unpack_multicontrol(buf: &[u8]) -> Result<Unpacked, McError> {
    if buf.len() < MC_PAYLOAD_LEN { return Err(McError::BadCount); }
    let b0 = buf[0];
    if b0 & MC_MAGIC_MASK != MC_MAGIC { return Err(McError::BadMagic); }
    let count = (b0 >> 4) & 0x03;
    let flags = b0 & 0x0F;
    if count == 0 || count as usize > MC_MAX_COUNT { return Err(McError::BadCount); }
    if flags != 0 { return Err(McError::ReservedFlags); }
    let mut cmds = Vec::with_capacity(count as usize);
    for i in 0..count as usize {
        cmds.push(Cmd::decode(buf[1 + i])?);
    }
    for i in count as usize..MC_MAX_COUNT {
        if buf[1 + i] != 0 { return Err(McError::UnusedSlotNonzero); }
    }
    Ok(Unpacked { count, flags, cmds })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn anchor() {
        let a = pack_multicontrol(&[Cmd::new(0, OP_NOP, 0).unwrap()], 0).unwrap();
        assert_eq!(a, [0xD0, 0, 0, 0]);
        assert!(is_multicontrol(&a));
        let u = unpack_multicontrol(&a).unwrap();
        assert_eq!(u.count, 1);
        assert_eq!(u.cmds, vec![Cmd::new(0, OP_NOP, 0).unwrap()]);
    }

    #[test]
    fn opcode_sweep() {
        for &op in &[OP_NOP, OP_CREDIT_DELTA, OP_ACK_CUMUL, OP_ACK_SELECTIVE,
                     OP_NACK_ONE, OP_DONE_HINT, OP_ABORT_HINT] {
            let c = Cmd::new(1, op, 1).unwrap();
            let v = pack_multicontrol(&[c], 0).unwrap();
            let u = unpack_multicontrol(&v).unwrap();
            assert_eq!(u.cmds[0], c);
        }
    }

    #[test]
    fn count_sweep() {
        let trio = [
            Cmd::new(0, OP_CREDIT_DELTA, 1).unwrap(),
            Cmd::new(2, OP_ACK_CUMUL, 0).unwrap(),
            Cmd::new(3, OP_ABORT_HINT, 1).unwrap(),
        ];
        for n in 1..=3 {
            let v = pack_multicontrol(&trio[..n], 0).unwrap();
            let u = unpack_multicontrol(&v).unwrap();
            assert_eq!(u.count as usize, n);
            assert_eq!(u.cmds, trio[..n]);
            for i in n..3 { assert_eq!(v[1 + i], 0); }
        }
    }

    #[test]
    fn reserved_rejected() {
        assert!(Cmd::new(0, OP_RESERVED, 0).is_err());
        let bad = [0xD0, OP_RESERVED << 3, 0, 0];
        assert!(unpack_multicontrol(&bad).is_err());
    }

    #[test]
    fn flags_rejected() {
        let bad = [0xD1, 0, 0, 0];
        assert!(unpack_multicontrol(&bad).is_err());
    }

    #[test]
    fn pad_bits_rejected() {
        let bad = [0xD0, 0b000_000_01, 0, 0];
        assert!(unpack_multicontrol(&bad).is_err());
    }

    #[test]
    fn unused_slot_nonzero_rejected() {
        let mut bad = [0u8; 4];
        bad[0] = 0xE0; // count=2
        bad[1] = Cmd::new(0, OP_NOP, 0).unwrap().encode();
        bad[2] = Cmd::new(0, OP_NOP, 0).unwrap().encode();
        bad[3] = 0x44; // unused slot nonzero
        assert!(unpack_multicontrol(&bad).is_err());
    }

    #[test]
    fn discriminator() {
        assert!(is_multicontrol(&[0xD0]));
        assert!(is_multicontrol(&[0xFF]));
        assert!(!is_multicontrol(&[0x00])); // classic OPEN
        assert!(!is_multicontrol(&[0x05])); // classic ABORT
        assert!(!is_multicontrol(&[0x7C])); // audio desc (124)
        assert!(is_classic_pipe(&[0x00]));
        assert!(is_classic_pipe(&[0x05]));
        assert!(!is_classic_pipe(&[0xD0]));
    }

    #[test]
    fn param_lsb_bit_position() {
        let p0 = pack_multicontrol(&[Cmd::new(0, OP_CREDIT_DELTA, 0).unwrap()], 0).unwrap();
        let p1 = pack_multicontrol(&[Cmd::new(0, OP_CREDIT_DELTA, 1).unwrap()], 0).unwrap();
        assert_eq!(p0[1] ^ p1[1], 0x04);
    }

    #[test]
    fn header_bytes() {
        assert_eq!(pack_multicontrol(&[Cmd::new(0, OP_NOP, 0).unwrap()], 0).unwrap()[0], 0xD0);
        let two = [Cmd::new(0, OP_NOP, 0).unwrap(), Cmd::new(0, OP_NOP, 0).unwrap()];
        assert_eq!(pack_multicontrol(&two, 0).unwrap()[0], 0xE0);
        let three = [two[0], two[1], Cmd::new(0, OP_NOP, 0).unwrap()];
        assert_eq!(pack_multicontrol(&three, 0).unwrap()[0], 0xF0);
    }
}