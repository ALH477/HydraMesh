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
 * Certifies {@link SuperPack} byte-for-byte against the cross-language golden
 * vectors in Documentation/superpack_vectors.json. Dependency-free (regex
 * extraction) so it runs with just a JDK.
 *
 * <p>Usage: {@code java com.demod.dcf.SuperPackCertify [path/to/superpack_vectors.json]}
 */
public final class SuperPackCertify {

    public static void main(String[] args) throws IOException {
        String json = new String(Files.readAllBytes(resolve(args)), StandardCharsets.UTF_8);
        int fails = 0;

        Pattern caseRe = Pattern.compile(
                "\"a\"\\s*:\\s*\"([0-9a-fA-F]+)\".*?\"b\"\\s*:\\s*\"([0-9a-fA-F]+)\""
                        + ".*?\"super\"\\s*:\\s*\"([0-9a-fA-F]+)\"",
                Pattern.DOTALL);
        Matcher m = caseRe.matcher(json);

        int n = 0;
        boolean packOk = true, unpackOk = true;
        String firstSuper = null;
        while (m.find()) {
            n++;
            byte[] a = unhex(m.group(1));
            byte[] b = unhex(m.group(2));
            String spHex = m.group(3).toLowerCase();
            if (firstSuper == null) {
                firstSuper = spHex;
            }
            byte[] packed = SuperPack.pack(a, b);
            if (packed.length != SuperPack.SUPER_LEN || !SuperPack.isSuperPack(packed)
                    || !Frame.hex(packed).equals(spHex)) {
                packOk = false;
            }
            byte[][] parts = SuperPack.unpack(unhex(spHex));
            if (!Frame.hex(parts[0]).equals(m.group(1).toLowerCase())
                    || !Frame.hex(parts[1]).equals(m.group(2).toLowerCase())) {
                unpackOk = false;
            }
        }
        fails += check(n > 0, "SuperPack vectors present");
        fails += check(packOk, n + " SuperPack pairs pack byte-identically");
        fails += check(unpackOk, n + " SuperPack pairs unpack losslessly");

        // joint CRC tamper detection on the first case
        if (firstSuper != null) {
            byte[] sp = unhex(firstSuper);
            int rejected = 0;
            for (int i = 0; i < sp.length; i++) {
                byte[] bad = sp.clone();
                bad[i] ^= 0x01;
                try {
                    SuperPack.unpack(bad);
                } catch (RuntimeException e) {
                    rejected++;
                }
            }
            fails += check(rejected == sp.length, "every single-bit flip rejected");
        }

        // zero-core anchor: SuperPack of two all-zero-core frames has joint CRC 0x5B75
        Frame zero = new Frame();
        zero.type = 0;
        byte[] zf = zero.encode();
        byte[] spz = SuperPack.pack(zf, zf);
        int joint = ((spz[30] & 0xFF) << 8) | (spz[31] & 0xFF);
        fails += check(joint == 0x5B75, String.format("zero-core joint CRC = 0x%04X (want 0x5B75)", joint));

        if (fails > 0) {
            System.err.println("\n" + fails + " CHECK(S) FAILED");
            System.exit(1);
        }
        System.out.println("\nALL SUPERPACK VECTORS HOLD — Java DCF SuperPack is cemented.");
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
            "Documentation/superpack_vectors.json",
            "../Documentation/superpack_vectors.json",
            "../../Documentation/superpack_vectors.json",
            "python/MCP/superpack_vectors.json",
        };
        List<Path> tried = new ArrayList<>();
        for (String c : candidates) {
            Path p = Path.of(c);
            if (Files.exists(p)) {
                return p;
            }
            tried.add(p);
        }
        throw new IllegalStateException("superpack_vectors.json not found; tried " + tried);
    }
}
