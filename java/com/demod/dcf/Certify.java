// SPDX-License-Identifier: LGPL-3.0-only
package com.demod.dcf;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Certifies {@link Frame} byte-for-byte against the cross-language golden vectors in
 * Documentation/golden_vectors.json. Dependency-free (regex extraction) so it runs
 * with just a JDK — no JUnit, no network.
 *
 * <p>Usage: {@code java com.demod.dcf.Certify [path/to/golden_vectors.json]}
 */
public final class Certify {

    public static void main(String[] args) throws IOException {
        String json = new String(Files.readAllBytes(resolve(args)), StandardCharsets.UTF_8);
        int fails = 0;

        // 1. CRC anchors
        int c1 = Frame.crc16("123456789".getBytes(StandardCharsets.US_ASCII));
        fails += check(c1 == 0x29B1, String.format("CRC(\"123456789\") = 0x%04X (want 0x29B1)", c1));
        int c0 = Frame.crc16(new byte[15]);
        fails += check(c0 == 0x4EC3, String.format("CRC(0^15) = 0x%04X (want 0x4EC3)", c0));

        // 2. example frame anchor
        String exHex = group(json, "\"exampleFrame_full\"\\s*:\\s*\"([0-9a-fA-F]+)\"");
        Frame ex = new Frame();
        ex.type = 3;
        ex.seq = 0x1234;
        ex.src = 1;
        ex.dst = Frame.BROADCAST;
        ex.payload = new byte[] {(byte) 0xDE, (byte) 0xAD, (byte) 0xBE, (byte) 0xEF};
        ex.tsUs = 0xAB12CD;
        String exGot = Frame.hex(ex.encode());
        fails += check(exGot.equalsIgnoreCase(exHex), "exampleFrame_full: got " + exGot + " want " + exHex);

        // 3. encode_basis: each frame raw-CRC-valid + (known types) decode/roundtrip
        List<String> frames = allGroups(json, "\"frame\"\\s*:\\s*\"([0-9a-fA-F]+)\"");
        fails += check(!frames.isEmpty(), "encode_basis vectors present");
        int encFails = 0;
        for (int i = 0; i < frames.size(); i++) {
            byte[] raw = unhex(frames.get(i));
            if (!rawValid(raw)) {
                encFails++;
                System.out.println("  FAIL  encode_basis[" + i + "]: raw CRC invalid");
                continue;
            }
            if ((raw[1] & 0x0F) <= 3) {
                try {
                    Frame f = Frame.decode(raw);
                    if (!Arrays.equals(f.encode(), raw)) {
                        encFails++;
                        System.out.println("  FAIL  encode_basis[" + i + "]: roundtrip mismatch");
                    }
                } catch (RuntimeException e) {
                    encFails++;
                    System.out.println("  FAIL  encode_basis[" + i + "]: " + e.getMessage());
                }
            }
        }
        fails += encFails;
        if (encFails == 0) {
            System.out.println("  PASS  " + frames.size() + " encode_basis vectors (decode + roundtrip)");
        }

        // 4. syndrome_basis: reproduce each basis word and check its syndrome
        String synSection = json.substring(json.indexOf("\"syndrome_basis\""));
        Matcher objs = Pattern.compile("\\{([^}]*)\\}").matcher(synSection);
        Pattern synPat = Pattern.compile("\"syndrome\"\\s*:\\s*(\\d+)");
        Pattern bitPat = Pattern.compile("\"bit\"\\s*:\\s*(\\d+)");
        int synCount = 0;
        int synFails = 0;
        while (objs.find()) {
            String obj = objs.group(1);
            Matcher sm = synPat.matcher(obj);
            if (!sm.find()) {
                continue; // not a syndrome entry
            }
            int expected = Integer.parseInt(sm.group(1)) & 0xFFFF;
            byte[] word = new byte[Frame.FRAME_SIZE];
            Matcher bm = bitPat.matcher(obj);
            if (bm.find()) {
                int bit = Integer.parseInt(bm.group(1));
                word[bit / 8] = (byte) (1 << (7 - (bit % 8)));
            }
            int got = Frame.syndrome(word);
            if (got != expected) {
                synFails++;
                System.out.println(String.format("  FAIL  syndrome_basis[%d]: got 0x%04X want 0x%04X", synCount, got, expected));
            }
            synCount++;
        }
        fails += check(synCount > 0, "syndrome_basis vectors present");
        fails += synFails;
        if (synFails == 0) {
            System.out.println("  PASS  " + synCount + " syndrome_basis vectors");
        }

        System.out.println();
        if (fails == 0) {
            System.out.println("ALL CHECKS PASSED — Java codec cemented (" + frames.size()
                    + " encode + " + synCount + " syndrome).");
            System.exit(0);
        } else {
            System.out.println(fails + " certification check(s) FAILED");
            System.exit(1);
        }
    }

    private static int check(boolean cond, String label) {
        System.out.println((cond ? "  PASS  " : "  FAIL  ") + label);
        return cond ? 0 : 1;
    }

    private static boolean rawValid(byte[] b) {
        if (b.length != Frame.FRAME_SIZE || (b[0] & 0xFF) != Frame.SYNC) {
            return false;
        }
        int stored = ((b[15] & 0xFF) << 8) | (b[16] & 0xFF);
        return Frame.crc16(b, Frame.CRC_COVER) == stored;
    }

    private static byte[] unhex(String s) {
        byte[] out = new byte[s.length() / 2];
        for (int i = 0; i < out.length; i++) {
            out[i] = (byte) Integer.parseInt(s.substring(2 * i, 2 * i + 2), 16);
        }
        return out;
    }

    private static String group(String s, String regex) {
        Matcher m = Pattern.compile(regex).matcher(s);
        if (!m.find()) {
            throw new IllegalStateException("not found: " + regex);
        }
        return m.group(1);
    }

    private static List<String> allGroups(String s, String regex) {
        Matcher m = Pattern.compile(regex).matcher(s);
        List<String> out = new ArrayList<>();
        while (m.find()) {
            out.add(m.group(1));
        }
        return out;
    }

    private static Path resolve(String[] args) {
        if (args.length > 0) {
            return Path.of(args[0]);
        }
        String[] candidates = {
            "Documentation/golden_vectors.json",
            "../Documentation/golden_vectors.json",
            "../../Documentation/golden_vectors.json",
            "python/MCP/golden_vectors.json",
        };
        for (String p : candidates) {
            Path path = Path.of(p);
            if (Files.exists(path)) {
                return path;
            }
        }
        throw new IllegalStateException("golden_vectors.json not found");
    }
}
