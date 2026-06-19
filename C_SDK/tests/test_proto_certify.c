/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * test_proto_certify.c — certifies the C ProtoMessage envelope against the
 * cross-language golden vector (the same one in go/node/proto_test.go), so the C
 * node's UDP datagrams are byte-identical to the Go/Rust reference SDKs.
 *
 * Build: gcc -std=c11 -I C_SDK/node C_SDK/tests/test_proto_certify.c -o test_proto
 */
#include <stdio.h>
#include <string.h>
#include "dcf_proto.h"

static int fails = 0;
static void hex(const uint8_t *b, size_t n, char *out) {
    static const char *d = "0123456789abcdef";
    for (size_t i = 0; i < n; i++) { out[2 * i] = d[b[i] >> 4]; out[2 * i + 1] = d[b[i] & 0xF]; }
    out[2 * n] = '\0';
}

int main(void) {
    /* golden: type=1 seq=42 ts=0x0102030405060708 payload=01 02 03 */
    uint8_t payload[3] = {0x01, 0x02, 0x03};
    uint8_t buf[64];
    size_t n = dcf_proto_serialize(1, 42, 0x0102030405060708ull, payload, 3, buf);
    char got[160];
    hex(buf, n, got);
    const char *want = "010000002a010203040506070800000003010203";
    if (strcmp(got, want) != 0) { fprintf(stderr, "FAIL serialize:\n got %s\nwant %s\n", got, want); fails++; }
    else printf("PASS: ProtoMessage golden vector\n");

    /* round-trip */
    uint8_t mt; uint32_t seq, plen; uint64_t ts; const uint8_t *p;
    if (!dcf_proto_deserialize(buf, n, &mt, &seq, &ts, &p, &plen)) { fprintf(stderr, "FAIL deserialize\n"); fails++; }
    else if (mt != 1 || seq != 42 || ts != 0x0102030405060708ull || plen != 3 || memcmp(p, payload, 3) != 0) {
        fprintf(stderr, "FAIL roundtrip fields\n"); fails++;
    } else printf("PASS: ProtoMessage round-trip\n");

    /* short header + overrun guards */
    if (dcf_proto_deserialize(buf, 10, &mt, &seq, &ts, &p, &plen)) { fprintf(stderr, "FAIL short-header guard\n"); fails++; }
    if (dcf_proto_deserialize(buf, n - 1, &mt, &seq, &ts, &p, &plen)) { fprintf(stderr, "FAIL overrun guard\n"); fails++; }
    if (fails == 0) printf("PASS: short/overrun guards\n\nALL PROTO VECTORS HOLD — C ProtoMessage is cemented.\n");
    return fails ? 1 : 0;
}
