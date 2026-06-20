// SPDX-License-Identifier: LGPL-3.0-only
package dcf

import java.io.File
import kotlin.system.exitProcess

/**
 * Certifies [Frame] byte-for-byte against the cross-language golden vectors in
 * Documentation/golden_vectors.json. Dependency-free (regex extraction) so it runs
 * with just the Kotlin compiler + a JVM — no JSON library, no test framework.
 *
 * Usage: `java -cp dcf.jar dcf.CertifyKt [path/to/golden_vectors.json]`
 * or, via Gradle: `gradle run` (from the `kotlin/` directory).
 */
fun main(args: Array<String>) {
    val json = File(resolveGolden(args)).readText()
    var fails = 0

    fun check(cond: Boolean, label: String): Int {
        println((if (cond) "  PASS  " else "  FAIL  ") + label)
        return if (cond) 0 else 1
    }

    // 1. CRC anchors
    val c1 = Frame.crc16("123456789".toByteArray(Charsets.US_ASCII))
    fails += check(c1 == 0x29B1, "CRC(\"123456789\") = 0x%04X (want 0x29B1)".format(c1))
    val c0 = Frame.crc16(ByteArray(15))
    fails += check(c0 == 0x4EC3, "CRC(0^15) = 0x%04X (want 0x4EC3)".format(c0))

    // 2. example frame anchor
    val exHex = Regex("\"exampleFrame_full\"\\s*:\\s*\"([0-9a-fA-F]+)\"").find(json)!!.groupValues[1]
    val ex = Frame(
        type = 3, seq = 0x1234, src = 1, dst = Frame.BROADCAST,
        payload = byteArrayOf(0xDE.toByte(), 0xAD.toByte(), 0xBE.toByte(), 0xEF.toByte()),
        tsUs = 0xAB12CD,
    )
    val exGot = Frame.hex(ex.encode())
    fails += check(exGot.equals(exHex, ignoreCase = true), "exampleFrame_full: got $exGot want $exHex")

    // 3. encode_basis: each frame raw-CRC-valid + (known types) decode/roundtrip
    val frames = Regex("\"frame\"\\s*:\\s*\"([0-9a-fA-F]+)\"").findAll(json).map { it.groupValues[1] }.toList()
    fails += check(frames.isNotEmpty(), "encode_basis vectors present")
    var encFails = 0
    for ((i, fr) in frames.withIndex()) {
        val raw = unhex(fr)
        if (!rawValid(raw)) {
            encFails++; println("  FAIL  encode_basis[$i]: raw CRC invalid"); continue
        }
        if ((raw[1].toInt() and 0x0F) <= 3) {
            try {
                val f = Frame.decode(raw)
                if (!f.encode().contentEquals(raw)) {
                    encFails++; println("  FAIL  encode_basis[$i]: roundtrip mismatch")
                }
            } catch (e: Exception) {
                encFails++; println("  FAIL  encode_basis[$i]: ${e.message}")
            }
        }
    }
    fails += encFails
    if (encFails == 0) println("  PASS  ${frames.size} encode_basis vectors (decode + roundtrip)")

    // 4. syndrome_basis: reproduce each basis word and check its syndrome
    val synSection = json.substring(json.indexOf("\"syndrome_basis\""))
    var synCount = 0
    var synFails = 0
    for (m in Regex("\\{([^}]*)\\}").findAll(synSection)) {
        val obj = m.groupValues[1]
        val sm = Regex("\"syndrome\"\\s*:\\s*(\\d+)").find(obj) ?: continue
        val expected = sm.groupValues[1].toInt() and 0xFFFF
        val word = ByteArray(Frame.FRAME_SIZE)
        Regex("\"bit\"\\s*:\\s*(\\d+)").find(obj)?.let {
            val bit = it.groupValues[1].toInt()
            word[bit / 8] = (1 shl (7 - bit % 8)).toByte()
        }
        val got = Frame.syndrome(word)
        if (got != expected) {
            synFails++
            println("  FAIL  syndrome_basis[$synCount]: got 0x%04X want 0x%04X".format(got, expected))
        }
        synCount++
    }
    fails += check(synCount > 0, "syndrome_basis vectors present")
    fails += synFails
    if (synFails == 0) println("  PASS  $synCount syndrome_basis vectors")

    // 5. SuperPack container vectors (Documentation/superpack_vectors.json)
    val spJson = File(resolveSuper()).readText()
    val spRe = Regex(
        "\"a\"\\s*:\\s*\"([0-9a-fA-F]+)\"[\\s\\S]*?\"b\"\\s*:\\s*\"([0-9a-fA-F]+)\"" +
            "[\\s\\S]*?\"super\"\\s*:\\s*\"([0-9a-fA-F]+)\""
    )
    val spCases = spRe.findAll(spJson).toList()
    fails += check(spCases.isNotEmpty(), "SuperPack vectors present")
    var spFails = 0
    for ((i, mc) in spCases.withIndex()) {
        val a = unhex(mc.groupValues[1]); val b = unhex(mc.groupValues[2])
        val sp = mc.groupValues[3].lowercase()
        val packed = SuperPack.pack(a, b)
        if (!SuperPack.isSuperPack(packed) || Frame.hex(packed) != sp) {
            spFails++; println("  FAIL  superpack[$i] pack")
        }
        val (ra, rb) = SuperPack.unpack(unhex(sp))
        if (Frame.hex(ra) != mc.groupValues[1].lowercase() || Frame.hex(rb) != mc.groupValues[2].lowercase()) {
            spFails++; println("  FAIL  superpack[$i] unpack")
        }
    }
    fails += spFails
    if (spFails == 0) println("  PASS  ${spCases.size} SuperPack pairs pack/unpack byte-identically")
    val zf = Frame(type = 0).encode()
    val spz = SuperPack.pack(zf, zf)
    val joint = ((spz[30].toInt() and 0xFF) shl 8) or (spz[31].toInt() and 0xFF)
    fails += check(joint == 0x5B75, "SuperPack zero-core joint CRC = 0x%04X (want 0x5B75)".format(joint))

    // 6. FEC adapter (Documentation/fec_vectors.json)
    val fecJson = File(resolveFec()).readText()
    var encOk = true; var nEnc = 0
    for (mc in Regex("\"msg\"\\s*:\\s*\"([0-9a-fA-F]+)\"\\s*,\\s*\"code\"\\s*:\\s*\"([0-9a-fA-F]+)\"").findAll(fecJson)) {
        nEnc++
        if (Frame.hex(FEC.rsEncode(unhex(mc.groupValues[1]), 16)) != mc.groupValues[2].lowercase()) encOk = false
    }
    fails += check(nEnc > 0 && encOk, "$nEnc RS encode vectors byte-identical")
    var decOk = true; var nDec = 0
    for (mc in Regex("\"corrupt\"\\s*:\\s*\"([0-9a-fA-F]+)\"\\s*,\\s*\"msg\"\\s*:\\s*\"([0-9a-fA-F]+)\"\\s*,\\s*\"nerr\"\\s*:\\s*(\\d+)").findAll(fecJson)) {
        nDec++
        val r = FEC.rsDecode(unhex(mc.groupValues[1]), 16, 17)
        if (Frame.hex(r.msg) != mc.groupValues[2].lowercase() || r.corrected != mc.groupValues[3].toInt()) decOk = false
    }
    fails += check(nDec > 0 && decOk, "$nDec corrupted codewords corrected")
    var msgOk = true; var nMsg = 0
    for (mc in Regex("\"len\"\\s*:\\s*\\d+\\s*,\\s*\"msg\"\\s*:\\s*\"([0-9a-fA-F]+)\"\\s*,\\s*\"blob\"\\s*:\\s*\"([0-9a-fA-F]+)\"").findAll(fecJson)) {
        nMsg++
        val msg = unhex(mc.groupValues[1])
        val blob = FEC.encodeMessage(msg, 16)
        if (Frame.hex(blob) != mc.groupValues[2].lowercase() || !FEC.decodeMessage(blob).msg.contentEquals(msg)) msgOk = false
    }
    fails += check(nMsg > 0 && msgOk, "$nMsg multi-codeword messages byte-identical + round-trip")
    val bm = Regex("\"message_burst\"\\s*:\\s*\\{[\\s\\S]*?\"msg\"\\s*:\\s*\"([0-9a-fA-F]+)\"[\\s\\S]*?\"corrupt\"\\s*:\\s*\"([0-9a-fA-F]+)\"").find(fecJson)
    val burstOk = bm != null && Frame.hex(FEC.decodeMessage(unhex(bm.groupValues[2])).msg) == bm.groupValues[1].lowercase()
    fails += check(burstOk, "interleaved burst across codewords corrected")

    println()
    if (fails == 0) {
        println("ALL CHECKS PASSED — Kotlin codec cemented (${frames.size} encode + $synCount syndrome).")
        exitProcess(0)
    } else {
        println("$fails certification check(s) FAILED")
        exitProcess(1)
    }
}

private fun rawValid(b: ByteArray): Boolean {
    if (b.size != Frame.FRAME_SIZE || (b[0].toInt() and 0xFF) != Frame.SYNC) return false
    val stored = ((b[15].toInt() and 0xFF) shl 8) or (b[16].toInt() and 0xFF)
    return Frame.crc16(b, Frame.CRC_COVER) == stored
}

private fun unhex(s: String): ByteArray =
    ByteArray(s.length / 2) { s.substring(it * 2, it * 2 + 2).toInt(16).toByte() }

private fun resolveGolden(args: Array<String>): String {
    if (args.isNotEmpty()) return args[0]
    for (p in listOf(
        "Documentation/golden_vectors.json",
        "../Documentation/golden_vectors.json",
        "../../Documentation/golden_vectors.json",
        "python/MCP/golden_vectors.json",
    )) {
        if (File(p).exists()) return p
    }
    error("golden_vectors.json not found")
}

private fun resolveSuper(): String {
    for (p in listOf(
        "Documentation/superpack_vectors.json",
        "../Documentation/superpack_vectors.json",
        "../../Documentation/superpack_vectors.json",
        "python/MCP/superpack_vectors.json",
    )) {
        if (File(p).exists()) return p
    }
    error("superpack_vectors.json not found (run gen_superpack_vectors.py)")
}

private fun resolveFec(): String {
    for (p in listOf(
        "Documentation/fec_vectors.json",
        "../Documentation/fec_vectors.json",
        "../../Documentation/fec_vectors.json",
        "python/MCP/fec_vectors.json",
    )) {
        if (File(p).exists()) return p
    }
    error("fec_vectors.json not found (run gen_fec_vectors.py)")
}
