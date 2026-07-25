/* SPDX-License-Identifier: LGPL-3.0-only
 *
 * test_streamdb_regress.c — first C-level test for lisp/streamdb: pins the
 * C5 fix (deleting the last key freed the trie root and left db->root
 * dangling; the next insert dereferenced it). Run under ASan to keep the
 * use-after-free half of the bug pinned, not just the crash.
 *
 * Built by CMake as test_streamdb_regress (compiles ../lisp/streamdb/streamdb.c).
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "streamdb.h"

static int g_fails = 0;
#define CHECK(cond, msg)                                                     \
    do {                                                                     \
        if (cond) {                                                          \
            printf("PASS: %s\n", msg);                                      \
        } else {                                                             \
            printf("FAIL: %s (%s:%d)\n", msg, __FILE__, __LINE__);          \
            g_fails++;                                                       \
        }                                                                    \
    } while (0)

int main(void) {
    const char* path = "streamdb_regress.tmp.db";
    unlink(path);

    StreamDB* db = streamdb_init(path, 0);
    CHECK(db != NULL, "init");
    if (!db) return 1;

    /* C5: insert -> delete the LAST key (frees the root) -> insert again.
     * Before the fix the second insert dereferenced the dangling root. */
    CHECK(streamdb_insert(db, (const unsigned char*)"k1", 2, "v1", 3) != 0,
          "insert first key");
    CHECK(streamdb_delete(db, (const unsigned char*)"k1", 2) != 0,
          "delete last remaining key");
    CHECK(streamdb_insert(db, (const unsigned char*)"k2", 2, "v2", 3) != 0,
          "insert after root-emptying delete (C5)");

    /* The re-rooted trie must actually serve reads. */
    Result* r = streamdb_prefix_search(db, (const unsigned char*)"k2", 2);
    CHECK(r != NULL && r->value_size == 3 && memcmp(r->value, "v2", 3) == 0,
          "search finds the post-C5 insert");
    streamdb_free_results(r);

    /* Repeat the empty->fill cycle a few times for good measure. */
    for (int i = 0; i < 5; i++) {
        unsigned char key[2] = { (unsigned char)('a' + i), 'x' };
        if (streamdb_insert(db, key, 2, "vv", 3) == 0) g_fails++;
        if (streamdb_delete(db, key, 2) == 0) g_fails++;
    }
    CHECK(streamdb_insert(db, (const unsigned char*)"zz", 2, "vz", 3) != 0,
          "insert after repeated empty/fill cycles");

    streamdb_free(db);
    unlink(path);

    if (g_fails == 0) printf("\nSTREAMDB C5 REGRESSION PINNED.\n");
    return g_fails ? 1 : 0;
}
