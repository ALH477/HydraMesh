// SPDX-License-Identifier: LGPL-3.0-only
package com.demod.dcf;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Certifies {@link FEC} byte-for-byte against the cross-language golden vectors in
 * Documentation/fec_vectors.json. Dependency-free (regex extraction).
 *
 * <p>Usage: {@code java com.demod.dcf.FECCertify [path/to/fec_vectors.json]}
 */
public final class FECCertify {

    public static void main(String[] args) throws IOException {
        String json = new String(Files.readAllBytes(resolve(args)), StandardCharsets.UTF_8);
        int fails = 0;
        final int np = 16;

        // 1. systematic encode byte-identical (cases: {"msg":..,"code":..})
        boolean encOk = true;
        int nEnc = 0;
        Matcher m = Pattern.compile("\"msg\"\\s*:\\s*\"([0-9a-fA-F]+)\"\\s*,\\s*\"code\"\\s*:\\s*\"([0-9a-fA-F]+)\"").matcher(json);
        while (m.find()) {
            nEnc++;
            if (!Frame.hex(FEC.rsEncode(unhex(m.group(1)), np)).equals(m.group(2).toLowerCase())) {
                encOk = false;
            }
        }
        fails += check(nEnc > 0 && encOk, nEnc + " RS encode vectors byte-identical");

        // 2. decode corrects (correct: {"corrupt":..,"msg":..,"nerr":..})
        boolean decOk = true;
        int nDec = 0;
        m = Pattern.compile("\"corrupt\"\\s*:\\s*\"([0-9a-fA-F]+)\"\\s*,\\s*\"msg\"\\s*:\\s*\"([0-9a-fA-F]+)\"\\s*,\\s*\"nerr\"\\s*:\\s*(\\d+)").matcher(json);
        while (m.find()) {
            nDec++;
            FEC.Result r = FEC.rsDecode(unhex(m.group(1)), np, 17);
            if (!Frame.hex(r.msg).equals(m.group(2).toLowerCase()) || r.corrected != Integer.parseInt(m.group(3))) {
                decOk = false;
            }
        }
        fails += check(nDec > 0 && decOk, nDec + " corrupted codewords corrected");

        // 3. multi-codeword messages (messages: {"len":..,"msg":..,"blob":..})
        boolean msgOk = true;
        int nMsg = 0;
        m = Pattern.compile("\"len\"\\s*:\\s*\\d+\\s*,\\s*\"msg\"\\s*:\\s*\"([0-9a-fA-F]+)\"\\s*,\\s*\"blob\"\\s*:\\s*\"([0-9a-fA-F]+)\"").matcher(json);
        while (m.find()) {
            nMsg++;
            byte[] msg = unhex(m.group(1));
            byte[] blob = FEC.encodeMessage(msg, np);
            if (!Frame.hex(blob).equals(m.group(2).toLowerCase())
                    || !java.util.Arrays.equals(FEC.decodeMessage(blob).msg, msg)) {
                msgOk = false;
            }
        }
        fails += check(nMsg > 0 && msgOk, nMsg + " multi-codeword messages byte-identical + round-trip");

        // 4. interleaved burst across codewords corrected (message_burst object)
        m = Pattern.compile("\"message_burst\"\\s*:\\s*\\{[^}]*?\"msg\"\\s*:\\s*\"([0-9a-fA-F]+)\"[^}]*?\"corrupt\"\\s*:\\s*\"([0-9a-fA-F]+)\"", Pattern.DOTALL).matcher(json);
        boolean burstOk = false;
        if (m.find()) {
            byte[] got = FEC.decodeMessage(unhex(m.group(2))).msg;
            burstOk = Frame.hex(got).equals(m.group(1).toLowerCase());
        }
        fails += check(burstOk, "interleaved burst across codewords corrected");

        if (fails > 0) {
            System.err.println("\n" + fails + " CHECK(S) FAILED");
            System.exit(1);
        }
        System.out.println("\nALL FEC VECTORS HOLD — Java DCF FEC is cemented.");
    }

    private static int check(boolean cond, String label) {
        System.out.println((cond ? "  PASS  " : "  FAIL  ") + label);
        return cond ? 0 : 1;
    }
    private static byte[] unhex(String s) {
        byte[] out = new byte[s.length() / 2];
        for (int i = 0; i < out.length; i++) {
            out[i] = (byte) Integer.parseInt(s.substring(i * 2, i * 2 + 2), 16);
        }
        return out;
    }
    private static Path resolve(String[] args) {
        if (args.length > 0) {
            return Path.of(args[0]);
        }
        String[] candidates = {
            "Documentation/fec_vectors.json", "../Documentation/fec_vectors.json",
            "../../Documentation/fec_vectors.json", "python/MCP/fec_vectors.json",
        };
        List<Path> tried = new ArrayList<>();
        for (String c : candidates) {
            Path p = Path.of(c);
            if (Files.exists(p)) {
                return p;
            }
            tried.add(p);
        }
        throw new IllegalStateException("fec_vectors.json not found; tried " + tried);
    }
}
