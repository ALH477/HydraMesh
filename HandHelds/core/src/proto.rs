// SPDX-License-Identifier: LGPL-3.0-only
//! DCF message envelope — the portable (de)serialiser for the handheld's app-level
//! message, independent of any platform. Carried on the wire by `wire.rs`.

use byteorder::{ByteOrder, LittleEndian};
use heapless::consts::*;
use heapless::{String, Vec};

pub const MAX_MSG_LEN: usize = 64;
/// Envelope is seq(4) + ts(8) + 4 NUL-terminated/embedded fields. Bounded buffer.
pub const MAX_ENVELOPE: usize = 124;

pub type Id = String<U32>;

#[derive(Clone, Default, PartialEq, Eq, Debug)]
pub struct DcfMessage {
    pub sender: Id,
    pub recipient: Id,
    pub data: Vec<u8, U64>,
    pub timestamp: u64,
    pub sync: bool,
    pub sequence: u32,
    pub redundancy_path: Id,
    pub group_id: Id,
}

fn push_cstr(buf: &mut Vec<u8, U128>, s: &str) {
    let _ = buf.extend_from_slice(s.as_bytes());
    let _ = buf.push(0);
}

/// Serialise an envelope: `seq(le u32) | ts(le u64) | sender\0 recipient\0 data\0
/// redundancy\0 group`. (`group` is the trailing field, no terminator.)
pub fn serialize(msg: &DcfMessage) -> Vec<u8, U128> {
    let mut buf: Vec<u8, U128> = Vec::new();
    let mut tmp = [0u8; 8];
    LittleEndian::write_u32(&mut tmp[..4], msg.sequence);
    let _ = buf.extend_from_slice(&tmp[..4]);
    LittleEndian::write_u64(&mut tmp, msg.timestamp);
    let _ = buf.extend_from_slice(&tmp);
    push_cstr(&mut buf, &msg.sender);
    push_cstr(&mut buf, &msg.recipient);
    let _ = buf.extend_from_slice(&msg.data);
    let _ = buf.push(0);
    push_cstr(&mut buf, &msg.redundancy_path);
    let _ = buf.extend_from_slice(msg.group_id.as_bytes());
    buf
}

fn read_cstr(data: &[u8], cursor: &mut usize) -> Option<Id> {
    if *cursor > data.len() {
        return None;
    }
    let rest = &data[*cursor..];
    let end = rest.iter().position(|&b| b == 0).unwrap_or(rest.len());
    let s = core::str::from_utf8(&rest[..end]).ok()?;
    let mut id: Id = String::new();
    id.push_str(s).ok()?;
    *cursor += end + 1; // skip the NUL (or past the end)
    Some(id)
}

/// Parse an envelope produced by [`serialize`]. Returns `None` on a malformed buffer.
pub fn parse(data: &[u8]) -> Option<DcfMessage> {
    if data.len() < 12 {
        return None;
    }
    let sequence = LittleEndian::read_u32(&data[0..4]);
    let timestamp = LittleEndian::read_u64(&data[4..12]);
    let mut cursor = 12usize;
    let sender = read_cstr(data, &mut cursor)?;
    let recipient = read_cstr(data, &mut cursor)?;
    // data field, NUL-terminated
    let rest = data.get(cursor..)?;
    let dend = rest.iter().position(|&b| b == 0).unwrap_or(rest.len());
    let mut payload: Vec<u8, U64> = Vec::new();
    payload.extend_from_slice(&rest[..dend.min(MAX_MSG_LEN)]).ok()?;
    cursor += dend + 1;
    let redundancy_path = read_cstr(data, &mut cursor)?;
    // trailing group_id (no terminator)
    let mut group_id: Id = String::new();
    if let Some(tail) = data.get(cursor..) {
        if let Ok(s) = core::str::from_utf8(tail) {
            group_id.push_str(s).ok()?;
        }
    }
    Some(DcfMessage {
        sender,
        recipient,
        data: payload,
        timestamp,
        sync: true,
        sequence,
        redundancy_path,
        group_id,
    })
}

/// The name a received `FILE:` transfer may be stored under, or `None`.
///
/// The name arrives in an unauthenticated UDP datagram, and the device used to
/// pass it straight to `storage.open(name, "wb+")`. So any sender on the Wi-Fi
/// could choose the path and overwrite any file, including `dcf.streamdb`,
/// which holds the device's config, peers and history. A received file is now
/// a bare name: 1..=32 bytes of `[A-Za-z0-9._-]`, not starting with '.', and
/// never the device's own database. That excludes '/', ':' (the DSi `fat:`
/// and PSP `ms0:` device prefixes) and every path-shaped thing at once.
pub fn received_file_name(name: &str) -> Option<&str> {
    let ok = !name.is_empty()
        && name.len() <= 32
        && !name.starts_with('.')
        && name
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || b == b'.' || b == b'_' || b == b'-')
        && !name.eq_ignore_ascii_case("dcf.streamdb");
    if ok {
        Some(name)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mk(data: &[u8]) -> DcfMessage {
        let mut m = DcfMessage::default();
        m.sender.push_str("host").unwrap();
        m.recipient.push_str("peer2").unwrap();
        m.data.extend_from_slice(data).unwrap();
        m.timestamp = 0x0102030405;
        m.sequence = 7;
        m.group_id.push_str("tictactoe_room").unwrap();
        m
    }

    #[test]
    fn roundtrip_identity() {
        let m = mk(b"MOVE:4");
        let bytes = serialize(&m);
        let back = parse(&bytes).expect("must parse");
        assert_eq!(back.sender, m.sender);
        assert_eq!(back.recipient, m.recipient);
        assert_eq!(back.data, m.data);
        assert_eq!(back.timestamp, m.timestamp);
        assert_eq!(back.sequence, m.sequence);
        assert_eq!(back.redundancy_path, m.redundancy_path);
        assert_eq!(back.group_id, m.group_id);
    }

    #[test]
    fn empty_data_roundtrips() {
        let m = mk(b"");
        let back = parse(&serialize(&m)).unwrap();
        assert_eq!(back.data.len(), 0);
        assert_eq!(back.group_id, m.group_id);
    }

    #[test]
    fn received_file_names_are_bare_and_never_the_database() {
        for ok in ["board.png", "save-1.dat", "a", "x_y.Z"] {
            assert_eq!(received_file_name(ok), Some(ok));
        }
        for bad in [
            "", "../dcf.streamdb", "/dcf.streamdb", "dcf.streamdb", "DCF.STREAMDB",
            "fat:/boot.nds", "ms0:/PSP/GAME/x", "a/b", ".hidden", "a b", "a\\b",
            "0123456789012345678901234567890123",
        ] {
            assert_eq!(received_file_name(bad), None, "{bad:?}");
        }
    }

    /// Every short or truncated datagram the platform's old hand-sliced parser
    /// panicked on is a clean `None` here.
    #[test]
    fn short_datagrams_are_none_not_a_panic() {
        for n in 0..12 {
            assert!(parse(&[0xAAu8; 12][..n]).is_none(), "{n}");
        }
        // Every prefix of a real envelope, and every single-byte change to
        // it: parse returns, whatever it returns.
        let full = serialize(&mk(b"MOVE:4"));
        for n in 0..=full.len() {
            let _ = parse(&full[..n]);
        }
        for i in 0..full.len() {
            for v in [0u8, 0xff, b'A'] {
                let mut m = full.clone();
                m[i] = v;
                let _ = parse(&m);
            }
        }
    }

    #[test]
    fn truncated_buffer_is_none() {
        assert!(parse(&[0u8; 4]).is_none());
        assert!(parse(&[]).is_none());
    }

    #[test]
    fn seq_and_ts_little_endian() {
        let m = mk(b"x");
        let b = serialize(&m);
        assert_eq!(LittleEndian::read_u32(&b[0..4]), 7);
        assert_eq!(LittleEndian::read_u64(&b[4..12]), 0x0102030405);
    }
}
