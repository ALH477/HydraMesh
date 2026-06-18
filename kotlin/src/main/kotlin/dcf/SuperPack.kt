// SPDX-License-Identifier: LGPL-3.0-only
package dcf

/**
 * DCF SuperPack — a 32-byte container that losslessly carries TWO 17-byte
 * DeModFrame quanta under a single joint CRC-16.
 *
 * Two raw frames cost 2*17 = 34 bytes. When frames are emitted in pairs the
 * second header is largely recoverable from context (both inner sync bytes are
 * 0xD3; each inner CRC is a pure function of its own 15 leading bytes). SuperPack
 * drops those 6 redundant bytes and spends 4 back on one outer sync, a
 * type/version tag, and ONE joint CRC over the whole container — net 34 -> 32
 * bytes plus a strictly stronger integrity check.
 *
 * Why it is the lower-latency option: a SuperPack puts a frame pair on the wire
 * as a single datagram instead of two — one packet, one IP/UDP header, one
 * syscall — so paired traffic crosses the network with strictly lower per-pair
 * overhead and latency than emitting the two frames separately.
 *
 * Unpack reconstructs each inner frame bit-exact, so the outputs are ordinary
 * valid DeModFrames and the 246-vector wire certificate is untouched.
 */
object SuperPack {
    const val SUPER_TYPE = 0x05
    const val SUPER_LEN = 32
    const val SUPER_CORE_LEN = 14 // a frame's bytes [1..14]
    const val SUPER_SFLAGS = (Frame.VERSION shl 4) or SUPER_TYPE

    /** The 14 reconstructable bytes of a frame; validates sync, version, inner CRC. */
    private fun frameCore(frame: ByteArray): ByteArray {
        require(frame.size == Frame.FRAME_SIZE) { "need a 17-byte frame" }
        require(frame[0].toInt() and 0xFF == Frame.SYNC) { "bad sync byte" }
        require((frame[1].toInt() and 0xFF) ushr 4 == Frame.VERSION) { "bad version nibble" }
        val stored = ((frame[15].toInt() and 0xFF) shl 8) or (frame[16].toInt() and 0xFF)
        require(Frame.crc16(frame, Frame.CRC_COVER) == stored) { "inner frame CRC mismatch" }
        return frame.copyOfRange(1, 1 + SUPER_CORE_LEN)
    }

    /** Restore a full 17-byte frame from its 14-byte core (sync + recomputed CRC). */
    private fun rebuildFrame(buf: ByteArray, off: Int): ByteArray {
        val f = ByteArray(Frame.FRAME_SIZE)
        f[0] = Frame.SYNC.toByte()
        System.arraycopy(buf, off, f, 1, SUPER_CORE_LEN)
        val crc = Frame.crc16(f, Frame.CRC_COVER)
        f[15] = (crc ushr 8).toByte()
        f[16] = crc.toByte()
        return f
    }

    /** Combine two valid 17-byte frames into one 32-byte SuperPack. */
    fun pack(a: ByteArray, b: ByteArray): ByteArray {
        val coreA = frameCore(a)
        val coreB = frameCore(b)
        val out = ByteArray(SUPER_LEN)
        out[0] = Frame.SYNC.toByte()
        out[1] = SUPER_SFLAGS.toByte()
        System.arraycopy(coreA, 0, out, 2, SUPER_CORE_LEN)
        System.arraycopy(coreB, 0, out, 2 + SUPER_CORE_LEN, SUPER_CORE_LEN)
        val crc = Frame.crc16(out, 30)
        out[30] = (crc ushr 8).toByte()
        out[31] = crc.toByte()
        return out
    }

    /** True iff buf looks like a SuperPack (length + sync + version/type tag). */
    fun isSuperPack(buf: ByteArray): Boolean =
        buf.size == SUPER_LEN && (buf[0].toInt() and 0xFF) == Frame.SYNC &&
            (buf[1].toInt() and 0xFF) == SUPER_SFLAGS

    /** Split a 32-byte SuperPack into Pair(frameA, frameB), each a bit-exact frame. */
    fun unpack(buf: ByteArray): Pair<ByteArray, ByteArray> {
        require(buf.size == SUPER_LEN) { "length != 32" }
        require(buf[0].toInt() and 0xFF == Frame.SYNC) { "bad sync byte" }
        require((buf[1].toInt() and 0xFF) ushr 4 == Frame.VERSION) { "bad version nibble" }
        require(buf[1].toInt() and 0x0F == SUPER_TYPE) { "not a SuperPack type" }
        val stored = ((buf[30].toInt() and 0xFF) shl 8) or (buf[31].toInt() and 0xFF)
        require(Frame.crc16(buf, 30) == stored) { "SuperPack CRC mismatch" }
        val frameA = rebuildFrame(buf, 2)
        val frameB = rebuildFrame(buf, 2 + SUPER_CORE_LEN)
        Frame.decode(frameA) // belt and braces: must decode cleanly
        Frame.decode(frameB)
        return Pair(frameA, frameB)
    }
}
