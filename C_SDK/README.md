# DCF - DeMoD Communication Framework (C SDK)

**0.x — pre-release, in active development**

The C SDK is one of the **Certified** wire-codec implementations (golden-vector
verified in CI via the `certify-c` job). The shipping build is deliberately
narrow: only **four modules compile and install** — `dcf_platform`, `dcf_error`,
`dcf_ringbuf`, `dcf_connpool` (the `DCF_SOURCES` list in `CMakeLists.txt`). These
provide the low-level primitives below.

> **What ships vs. what is declared.** The public headers and the source tree
> declare more than the build delivers. Anything under `include/experimental/`,
> `plugins/experimental/`, and `tests/legacy/` is **quarantined and does not
> build** — including the high-level client API (`dcf_client_*`). Do not rely on
> those symbols; the examples below use only the four compiled modules.

## Features (shipping modules)

- **Thread-Safe Memory Management** - Tracked allocations with leak detection
- **Lock-Free Ring Buffers** - SPSC and MPMC modes with backpressure support  
- **Connection Pooling** - Per-peer limits, health checking, automatic eviction
- **Circuit Breaker Pattern** - Fault isolation with configurable thresholds
- **Retry Logic** - Exponential backoff with jitter
- **Comprehensive Error Handling** - Error chaining, stack traces, categorization
- **Cross-Platform** - Linux, macOS, Windows, FreeBSD
- **Wire-quantum certification** - `tests/test_wire_certify.c` certifies the
  17-byte `DeModFrame` codec (`dcf_frame_t` / `dcf_crc16`) against the
  cross-language golden vectors.

## Quick Start

### Using Nix (Recommended)

```bash
# Build and run tests
nix build
./result/bin/dcf_tests

# Development shell
nix develop
```

### Using Docker

```bash
# Build and run
docker build -t dcf:latest .
docker run --rm dcf:latest

# Run tests
docker compose run --rm test

# Development environment
docker compose run --rm dev
```

### Using CMake

```bash
cd C_SDK && mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
ctest --output-on-failure
sudo make install
```

### Wire-quantum certification

The wire codec is certified against the cross-language golden vectors with a
dependency-free test (this is what the `certify-c` CI job runs). From the repo
root:

```bash
gcc -std=c11 -Wall -Wextra -I codec C_SDK/tests/test_wire_certify.c -lm -o /tmp/wire_certify
/tmp/wire_certify
```

## Build Options

| Option | Default | Description |
|--------|---------|-------------|
| `DCF_BUILD_SHARED` | ON | Build shared library |
| `DCF_BUILD_STATIC` | ON | Build static library |
| `DCF_BUILD_TESTS` | ON | Build unit tests |
| `DCF_ENABLE_SANITIZERS` | OFF | Enable ASan/UBSan |
| `DCF_ENABLE_COVERAGE` | OFF | Enable code coverage |

## Usage Examples

### Ring Buffer

```c
#include <dcf/dcf_ringbuf.h>

DCFRingBufConfig cfg = {
    .capacity = 4096,
    .item_size = sizeof(int),
    .mode = DCF_RINGBUF_MPMC,
    .overflow = DCF_RINGBUF_DROP_NEW
};

DCFRingBuf* rb = dcf_ringbuf_create(&cfg);

int val = 42;
dcf_ringbuf_write(rb, &val);
dcf_ringbuf_read(rb, &val);

dcf_ringbuf_destroy(rb);
```

### Connection Pool with Circuit Breaker

```c
#include <dcf/dcf_connpool.h>

DCFConnPoolConfig cfg = DCF_CONNPOOL_CONFIG_DEFAULT;
cfg.factory = my_connection_factory;
cfg.max_connections = 100;
cfg.circuit.failure_threshold = 5;

DCFConnPool* pool = dcf_connpool_create(&cfg);
dcf_connpool_start(pool);

DCFPooledConn* conn = dcf_connpool_acquire(pool, "server1", 5000);
if (conn) {
    // Use connection...
    dcf_connpool_release(pool, conn, true);
}

dcf_connpool_destroy(pool, true);
```

## Docker Commands

```bash
docker compose run --rm test        # Run tests
docker compose run --rm test-asan   # Run with AddressSanitizer
docker compose run --rm coverage    # Generate coverage report
docker compose run --rm dev         # Development shell
```

## Nix Commands

```bash
nix build .#dcf            # Release build
nix build .#dcf-debug      # Debug build
nix build .#dcf-sanitized  # With sanitizers
nix flake check            # Run all checks
nix develop                # Enter dev shell
```

## License

**LGPL-3.0** for the linkable library. (GPL-3.0 in the wider monorepo is scoped
to the bundled DOOM example only.)
