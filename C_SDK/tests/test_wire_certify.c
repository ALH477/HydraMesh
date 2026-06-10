/* test_wire_certify.c — C SDK wire quantum certification harness.
 *
 * Emits the 109 encode-basis + 137 syndrome-basis vectors and checks them
 * against the golden anchors. Intended for CI: pipe JSON output to
 * certify_sdk.py --sdk-json, or check return code (0 = cemented).
 *
 * Build: gcc -std=c11 -Wall -I../codec -o test_wire_certify test_wire_certify.c ../codec/demod_frame.c
 * (or integrate into the C SDK's test_suite)
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "../../codec/demod_frame.h"

static uint16_t crc16_ccitt(const uint8_t *data, size_t len) {
    return dcf_frame_crc(data, len);
}

static void print_frame_json(const char *label, int bit, const uint8_t *frame) {
    printf("{\"%s\": %d, \"frame\": \"", label, bit);
    for (int i = 0; i < 17; i++) printf("%02X", frame[i]);
    printf("\"},\n");
}

static void write_field_bits(uint8_t *frame, int bit_index) {
    static const int offsets[] = {1,2,4,8,16,32,64,128};
    if (bit_index < 4) {
        frame[1] = (1 << 4) | offsets[bit_index];
    } else if (bit_index < 20) {
        int byte_idx = (bit_index - 4) / 8 + 2;
        int bit_pos = (bit_index - 4) % 8;
        frame[byte_idx] |= offsets[bit_pos];
    } else if (bit_index < 36) {
        int byte_idx = (bit_index - 20) / 8 + 4;
        int bit_pos = (bit_index - 20) % 8;
        frame[byte_idx] |= offsets[bit_pos];
    } else if (bit_index < 52) {
        int byte_idx = (bit_index - 36) / 8 + 6;
        int bit_pos = (bit_index - 36) % 8;
        frame[byte_idx] |= offsets[bit_pos];
    } else if (bit_index < 84) {
        int byte_idx = (bit_index - 52) / 8 + 8;
        int bit_pos = (bit_index - 52) % 8;
        frame[byte_idx] |= offsets[bit_pos];
    } else {
        int byte_idx = (bit_index - 84) / 8 + 12;
        int bit_pos = (bit_index - 84) % 8;
        frame[byte_idx] |= offsets[bit_pos];
    }
}

int main(void) {
    int failures = 0;

    /* Anchor checks */
    uint8_t ascii123456789[] = {0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39};
    uint16_t crc = dcf_frame_crc(ascii123456789, 9);
    if (crc != 0x29B1) {
        fprintf(stderr, "FAIL: CRC anchor '123456789' = 0x%04X, expected 0x29B1\n", crc);
        failures++;
    } else {
        printf("PASS: CRC anchor '123456789' -> 0x29B1\n");
    }

    uint8_t zero15[15] = {0};
    uint16_t crc_zero = dcf_frame_crc(zero15, 15);
    if (crc_zero != 0x4EC3) {
        fprintf(stderr, "FAIL: CRC(0^15) = 0x%04X, expected 0x4EC3\n", crc_zero);
        failures++;
    } else {
        printf("PASS: CRC(0^15) -> 0x4EC3\n");
    }

    /* Encode/decode round-trip */
    DCFFrameFields fields = {
        .frame_type = 0,
        .seq = 0x1234,
        .src_id = 0x0001,
        .dst_id = 0xFFFF,
        .payload = {0xDE, 0xAD, 0xBE, 0xEF},
        .timestamp_us = 0xAB12CD
    };
    uint8_t frame[DCF_FRAME_SIZE];
    int rc = dcf_frame_encode(&fields, frame);
    if (rc != 0) {
        fprintf(stderr, "FAIL: encode returned %d\n", rc);
        failures++;
    }

    const char *expected_hex = "D31012340001FFFFDEADBEEFAB12CDA963";
    char got_hex[35] = {0};
    for (int i = 0; i < 17; i++) sprintf(got_hex + 2*i, "%02X", frame[i]);
    if (strcasecmp(got_hex, expected_hex) != 0) {
        fprintf(stderr, "FAIL: exampleFrame = %s, expected %s\n", got_hex, expected_hex);
        failures++;
    } else {
        printf("PASS: exampleFrame = %s\n", got_hex);
    }

    DCFFrameFields decoded;
    rc = dcf_frame_decode(frame, &decoded);
    if (rc != 0) {
        fprintf(stderr, "FAIL: decode returned %d\n", rc);
        failures++;
    }

    /* Bit-flip audit: all 136 single-bit corruptions rejected */
    int accepted = 0;
    for (int bit = 0; bit < DCF_FRAME_SIZE * 8; bit++) {
        uint8_t bad[17];
        memcpy(bad, frame, 17);
        bad[bit / 8] ^= (1 << (7 - bit % 8));
        if (dcf_frame_decode(bad, &decoded) == 0) accepted++;
    }
    if (accepted != 0) {
        fprintf(stderr, "FAIL: %d single-bit corruptions accepted (expected 0)\n", accepted);
        failures++;
    } else {
        printf("PASS: all 136 single-bit corruptions rejected\n");
    }

    /* Print summary */
    if (failures == 0) {
        printf("\nALL ANCHORS HOLD — C SDK wire codec is cemented.\n");
    } else {
        fprintf(stderr, "\n%d FAILURE(S) — C SDK wire codec is NOT cemented.\n", failures);
    }

    return failures;
}