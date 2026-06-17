// SPDX-License-Identifier: LGPL-3.0-only
package com.demod.dcf;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/**
 * The DeMoD 17-byte DeModFrame wire quantum (version 1), byte-identical to the
 * reference codec in python/MCP/wirelab_core.py and certified against
 * Documentation/golden_vectors.json (the cross-language contract).
 *
 * <pre>
 * Wire layout (big-endian):
 *   [0]     sync = 0xD3
 *   [1]     flags: version[7:4]=1 | frame_type[3:0]
 *   [2:4]   seq      u16
 *   [4:6]   src      u16
 *   [6:8]   dst      u16
 *   [8:12]  payload  4 bytes
 *   [12:15] ts_us    u24
 *   [15:17] CRC-16/CCITT-FALSE over bytes [0..14]
 * </pre>
 */
public final class Frame {
    public static final int SYNC = 0xD3;
    public static final int VERSION = 1;
    public static final int FRAME_SIZE = 17;
    public static final int CRC_COVER = 15;
    public static final int BROADCAST = 0xFFFF;

    public int version = 1;            // 4-bit
    public int type;                   // 4-bit (0=Data,1=Ack,2=Beacon,3=Ctrl)
    public int seq;                    // u16
    public int src;                    // u16
    public int dst;                    // u16
    public byte[] payload = new byte[4];
    public int tsUs;                   // u24

    /** CRC-16/CCITT-FALSE (poly 0x1021, init 0xFFFF, no reflection, no xorout). */
    public static int crc16(byte[] data, int len) {
        int crc = 0xFFFF;
        for (int k = 0; k < len; k++) {
            crc ^= (data[k] & 0xFF) << 8;
            for (int i = 0; i < 8; i++) {
                if ((crc & 0x8000) != 0) {
                    crc = ((crc << 1) ^ 0x1021) & 0xFFFF;
                } else {
                    crc = (crc << 1) & 0xFFFF;
                }
            }
        }
        return crc & 0xFFFF;
    }

    public static int crc16(byte[] data) {
        return crc16(data, data.length);
    }

    /** Serialise into exactly 17 bytes, computing and appending the CRC. */
    public byte[] encode() {
        byte[] b = new byte[FRAME_SIZE];
        b[0] = (byte) SYNC;
        b[1] = (byte) (((version & 0x0F) << 4) | (type & 0x0F));
        b[2] = (byte) (seq >>> 8);
        b[3] = (byte) seq;
        b[4] = (byte) (src >>> 8);
        b[5] = (byte) src;
        b[6] = (byte) (dst >>> 8);
        b[7] = (byte) dst;
        System.arraycopy(payload, 0, b, 8, 4);
        b[12] = (byte) ((tsUs >>> 16) & 0xFF);
        b[13] = (byte) ((tsUs >>> 8) & 0xFF);
        b[14] = (byte) (tsUs & 0xFF);
        int crc = crc16(b, CRC_COVER);
        b[15] = (byte) (crc >>> 8);
        b[16] = (byte) crc;
        return b;
    }

    /** Affine validity syndrome of a 17-byte word: CRC-valid iff this returns 0. */
    public static int syndrome(byte[] w) {
        if (w.length != FRAME_SIZE) {
            throw new IllegalArgumentException("need 17 bytes");
        }
        int stored = ((w[15] & 0xFF) << 8) | (w[16] & 0xFF);
        return (crc16(w, CRC_COVER) ^ stored) & 0xFFFF;
    }

    /** Parse a 17-byte buffer, validating sync, version nibble, and CRC. */
    public static Frame decode(byte[] w) {
        if (w.length != FRAME_SIZE) {
            throw new IllegalArgumentException("length != 17");
        }
        if ((w[0] & 0xFF) != SYNC) {
            throw new IllegalArgumentException("bad sync byte");
        }
        if (((w[1] & 0xFF) >>> 4) != VERSION) {
            throw new IllegalArgumentException("bad version nibble");
        }
        if (syndrome(w) != 0) {
            throw new IllegalArgumentException("CRC mismatch");
        }
        Frame f = new Frame();
        f.version = (w[1] & 0xFF) >>> 4;
        f.type = w[1] & 0x0F;
        f.seq = ((w[2] & 0xFF) << 8) | (w[3] & 0xFF);
        f.src = ((w[4] & 0xFF) << 8) | (w[5] & 0xFF);
        f.dst = ((w[6] & 0xFF) << 8) | (w[7] & 0xFF);
        f.payload = Arrays.copyOfRange(w, 8, 12);
        f.tsUs = ((w[12] & 0xFF) << 16) | ((w[13] & 0xFF) << 8) | (w[14] & 0xFF);
        return f;
    }

    /** Lowercase hex of a byte array. */
    public static String hex(byte[] b) {
        StringBuilder sb = new StringBuilder(b.length * 2);
        for (byte x : b) {
            sb.append(Character.forDigit((x >> 4) & 0xF, 16));
            sb.append(Character.forDigit(x & 0xF, 16));
        }
        return sb.toString();
    }

    // Golden anchor frame (Ctrl, seq=0x1234, src=1, dst=broadcast, payload=DEADBEEF,
    // ts=0xAB12CD) from Documentation/golden_vectors.json.
    private static final String EXAMPLE_FRAME_FULL = "d31312340001ffffdeadbeefab12cd24c0";

    // Self-certify on class load: refuse to load if the codec has diverged from the
    // reference (CRC + example-frame anchors).
    static {
        int c1 = crc16("123456789".getBytes(StandardCharsets.US_ASCII));
        if (c1 != 0x29B1) {
            throw new IllegalStateException(String.format("CRC anchor diverged: CRC(\"123456789\")=0x%04X, want 0x29B1", c1));
        }
        int c0 = crc16(new byte[15]);
        if (c0 != 0x4EC3) {
            throw new IllegalStateException(String.format("CRC anchor diverged: CRC(0^15)=0x%04X, want 0x4EC3", c0));
        }
        Frame ex = new Frame();
        ex.type = 3;
        ex.seq = 0x1234;
        ex.src = 1;
        ex.dst = BROADCAST;
        ex.payload = new byte[] {(byte) 0xDE, (byte) 0xAD, (byte) 0xBE, (byte) 0xEF};
        ex.tsUs = 0xAB12CD;
        String got = hex(ex.encode());
        if (!got.equals(EXAMPLE_FRAME_FULL)) {
            throw new IllegalStateException("exampleFrame anchor diverged: got " + got);
        }
    }
}
