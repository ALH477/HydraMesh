/* test_wire_certify.c — C SDK wire quantum certification harness.
 *
 * Checks the C reference codec (codec/demod_frame.h) against the published CRC
 * anchors and the cross-language example frame, then audits that every single-bit
 * corruption is rejected. Return code 0 = cemented.
 *
 * Build (CI): gcc -std=c11 -Wall -Wextra -I codec -o wire_certify \
 *               C_SDK/tests/test_wire_certify.c -lm
 */
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include "../../codec/demod_frame.h"

int main(void) {
    int failures = 0;

    /* ── Anchor checks (the cross-language CRC fixtures) ── */
    uint8_t ascii123456789[] = {0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39};
    uint16_t crc = dcf_crc16(ascii123456789, 9);
    if (crc != 0x29B1) {
        fprintf(stderr, "FAIL: CRC anchor '123456789' = 0x%04X, expected 0x29B1\n", crc);
        failures++;
    } else {
        printf("PASS: CRC anchor '123456789' -> 0x29B1\n");
    }

    uint8_t zero15[15] = {0};
    uint16_t crc_zero = dcf_crc16(zero15, 15);
    if (crc_zero != 0x4EC3) {
        fprintf(stderr, "FAIL: CRC(0^15) = 0x%04X, expected 0x4EC3\n", crc_zero);
        failures++;
    } else {
        printf("PASS: CRC(0^15) -> 0x4EC3\n");
    }

    /* ── Encode the canonical example frame and check it byte-for-byte ── */
    dcf_frame_t fields;
    dcf_frame_init(&fields, /*version*/ 1, DCF_TYPE_DATA,
                   /*seq*/ 0x1234, /*src*/ 0x0001, /*dst*/ 0xFFFF);
    fields.payload[0] = 0xDE; fields.payload[1] = 0xAD;
    fields.payload[2] = 0xBE; fields.payload[3] = 0xEF;
    fields.timestamp_us = 0xAB12CD;

    uint8_t frame[DCF_FRAME_SIZE];
    dcf_frame_encode(&fields, frame);

    const char *expected_hex = "D31012340001FFFFDEADBEEFAB12CDA963";
    char got_hex[2 * DCF_FRAME_SIZE + 1] = {0};
    for (int i = 0; i < (int)DCF_FRAME_SIZE; i++) sprintf(got_hex + 2 * i, "%02X", frame[i]);
    if (strcmp(got_hex, expected_hex) != 0) {  /* both sides are uppercase hex */
        fprintf(stderr, "FAIL: exampleFrame = %s, expected %s\n", got_hex, expected_hex);
        failures++;
    } else {
        printf("PASS: exampleFrame = %s\n", got_hex);
    }

    /* ── Round-trip decode must succeed and recover the fields ── */
    dcf_frame_t decoded;
    if (!dcf_frame_decode(frame, &decoded)) {
        fprintf(stderr, "FAIL: decode of a valid frame returned false\n");
        failures++;
    } else if (decoded.seq != fields.seq || decoded.src_id != fields.src_id ||
               decoded.dst_id != fields.dst_id || decoded.type != fields.type ||
               decoded.timestamp_us != fields.timestamp_us ||
               memcmp(decoded.payload, fields.payload, 4) != 0) {
        fprintf(stderr, "FAIL: decoded fields differ from encoded fields\n");
        failures++;
    } else {
        printf("PASS: encode/decode round-trip preserves all fields\n");
    }

    /* ── Bit-flip audit: all 136 single-bit corruptions rejected ── */
    int accepted = 0;
    for (int bit = 0; bit < (int)DCF_FRAME_SIZE * 8; bit++) {
        uint8_t bad[DCF_FRAME_SIZE];
        memcpy(bad, frame, DCF_FRAME_SIZE);
        bad[bit / 8] ^= (uint8_t)(1u << (7 - bit % 8));
        if (dcf_frame_decode(bad, &decoded)) accepted++;
    }
    if (accepted != 0) {
        fprintf(stderr, "FAIL: %d single-bit corruptions accepted (expected 0)\n", accepted);
        failures++;
    } else {
        printf("PASS: all %d single-bit corruptions rejected\n", (int)DCF_FRAME_SIZE * 8);
    }

    if (failures == 0) {
        printf("\nALL ANCHORS HOLD — C SDK wire codec is cemented.\n");
    } else {
        fprintf(stderr, "\n%d FAILURE(S) — C SDK wire codec is NOT cemented.\n", failures);
    }
    return failures != 0;
}
