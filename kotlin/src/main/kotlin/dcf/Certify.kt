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
