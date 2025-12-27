# DCF - Distributed Communication Framework

**Version 5.2.0 - Battle Tested Edition**

A production-ready C SDK for distributed communication with comprehensive reliability patterns, cross-platform support, and extensive testing infrastructure.

## Features

- **Thread-Safe Memory Management** - Tracked allocations with leak detection
- **Lock-Free Ring Buffers** - SPSC and MPMC modes with backpressure support  
- **Connection Pooling** - Per-peer limits, health checking, automatic eviction
- **Circuit Breaker Pattern** - Fault isolation with configurable thresholds
- **Retry Logic** - Exponential backoff with jitter
- **Comprehensive Error Handling** - Error chaining, stack traces, categorization
- **Cross-Platform** - Linux, macOS, Windows, FreeBSD
- **Production Logging** - File rotation, syslog, custom callbacks

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
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
ctest --output-on-failure
sudo make install
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

LGPL License
