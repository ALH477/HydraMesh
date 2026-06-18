// SPDX-License-Identifier: LGPL-3.0-only
package com.demod.dcf;

/**
 * DCF SuperPack — a 32-byte container that losslessly carries TWO 17-byte
 * DeModFrame quanta under a single joint CRC-16.
 *
 * <p>Two raw frames cost 2*17 = 34 bytes. When frames are emitted in pairs the
 * second header is largely recoverable from context (both inner sync bytes are
 * 0xD3; each inner CRC is a pure function of its own 15 leading bytes). SuperPack
 * drops those 6 redundant bytes and spends 4 back on one outer sync, a
 * type/version tag, and ONE joint CRC over the whole container — net 34 -&gt; 32
 * bytes plus a strictly stronger integrity check.
 *
 * <p>Why it is the lower-latency option: a SuperPack puts a frame pair on the
 * wire as a single datagram instead of two — one packet, one IP/UDP header, one
 * syscall — so paired traffic crosses the network with strictly lower per-pair
 * overhead and latency than emitting the two frames separately.
 *
 * <p>Unpack reconstructs each inner frame bit-exact, so the outputs are ordinary
 * valid DeModFrames and the 246-vector wire certificate is untouched.
 */
public final class SuperPack {

    public static final int SUPER_TYPE = 0x05;
    public static final int SUPER_LEN = 32;
    public static final int SUPER_CORE_LEN = 14; // a frame's bytes [1..14]
    public static final int SUPER_SFLAGS = (Frame.VERSION << 4) | SUPER_TYPE;

    private SuperPack() {}

    /** The 14 reconstructable bytes of a frame; validates sync, version, inner CRC. */
    private static byte[] frameCore(byte[] frame) {
        if (frame.length != Frame.FRAME_SIZE) {
            throw new IllegalArgumentException("need a 17-byte frame");
        }
        if ((frame[0] & 0xFF) != Frame.SYNC) {
            throw new IllegalArgumentException("bad sync byte");
        }
        if (((frame[1] & 0xFF) >>> 4) != Frame.VERSION) {
            throw new IllegalArgumentException("bad version nibble");
        }
        int stored = ((frame[15] & 0xFF) << 8) | (frame[16] & 0xFF);
        if (Frame.crc16(frame, Frame.CRC_COVER) != stored) {
            throw new IllegalArgumentException("inner frame CRC mismatch");
        }
        byte[] core = new byte[SUPER_CORE_LEN];
        System.arraycopy(frame, 1, core, 0, SUPER_CORE_LEN);
        return core;
    }

    /** Restore a full 17-byte frame from its 14-byte core (sync + recomputed CRC). */
    private static byte[] rebuildFrame(byte[] core, int off) {
        byte[] f = new byte[Frame.FRAME_SIZE];
        f[0] = (byte) Frame.SYNC;
        System.arraycopy(core, off, f, 1, SUPER_CORE_LEN);
        int crc = Frame.crc16(f, Frame.CRC_COVER);
        f[15] = (byte) (crc >>> 8);
        f[16] = (byte) crc;
        return f;
    }

    /** Combine two valid 17-byte frames into one 32-byte SuperPack. */
    public static byte[] pack(byte[] a, byte[] b) {
        byte[] coreA = frameCore(a);
        byte[] coreB = frameCore(b);
        byte[] out = new byte[SUPER_LEN];
        out[0] = (byte) Frame.SYNC;
        out[1] = (byte) SUPER_SFLAGS;
        System.arraycopy(coreA, 0, out, 2, SUPER_CORE_LEN);
        System.arraycopy(coreB, 0, out, 2 + SUPER_CORE_LEN, SUPER_CORE_LEN);
        int crc = Frame.crc16(out, 30);
        out[30] = (byte) (crc >>> 8);
        out[31] = (byte) crc;
        return out;
    }

    /** True iff buf looks like a SuperPack (length + sync + version/type tag). */
    public static boolean isSuperPack(byte[] buf) {
        return buf.length == SUPER_LEN && (buf[0] & 0xFF) == Frame.SYNC
                && (buf[1] & 0xFF) == SUPER_SFLAGS;
    }

    /** Split a 32-byte SuperPack into [frameA, frameB], each a bit-exact frame. */
    public static byte[][] unpack(byte[] buf) {
        if (buf.length != SUPER_LEN) {
            throw new IllegalArgumentException("length != 32");
        }
        if ((buf[0] & 0xFF) != Frame.SYNC) {
            throw new IllegalArgumentException("bad sync byte");
        }
        if (((buf[1] & 0xFF) >>> 4) != Frame.VERSION) {
            throw new IllegalArgumentException("bad version nibble");
        }
        if ((buf[1] & 0x0F) != SUPER_TYPE) {
            throw new IllegalArgumentException("not a SuperPack type");
        }
        int stored = ((buf[30] & 0xFF) << 8) | (buf[31] & 0xFF);
        if (Frame.crc16(buf, 30) != stored) {
            throw new IllegalArgumentException("SuperPack CRC mismatch");
        }
        byte[] frameA = rebuildFrame(buf, 2);
        byte[] frameB = rebuildFrame(buf, 2 + SUPER_CORE_LEN);
        Frame.decode(frameA); // belt and braces: must decode cleanly
        Frame.decode(frameB);
        return new byte[][] {frameA, frameB};
    }
}
