# C SDK for DeMoD Communications Framework
Version 5.0.0 | August 25, 2025

## Overview
The C SDK provides a modular, low-latency (sub-1ms RTT) implementation of the DeMoD Communications Framework (DCF) for client, server, peer-to-peer (P2P), and AUTO modes, leveraging gRPC and Protocol Buffers (Protobuf) for networking and serialization. Key features include:
- **RTT-Based Grouping**: Dynamic peer grouping based on round-trip time for efficient network optimization.
- **Valgrind-Compatible Memory Management**: Ensures robust memory safety for production use.
- **Plugin System**: Extensible via dynamic plugins (e.g., `libcustom.so`) for custom functionality.
- **AUTO Mode**: AI-driven dynamic role assignment for adaptive network topologies.
- **CLI and TUI**: Comprehensive command-line interface (CLI) and ncurses-based text user interface (TUI) for operation and monitoring.

The SDK is part of the DCF monorepo at [github.com/ALH477/DeMoD-Communication-Framework](https://github.com/ALH477/DeMoD-Communication-Framework), which includes C, Python, and Perl SDKs. This README focuses on the C SDK (`c_sdk/` directory).

## Prerequisites
- **System**: Linux (e.g., Alpine, Ubuntu, NixOS) or macOS.
- **Dependencies**:
  - `cmake` (>= 3.10): Build system.
  - `protobuf-c` (>= 1.4.1): C bindings for Protobuf serialization.
  - `libuuid` (>= 2.39): UUID generation for node IDs.
  - `cjson` (>= 1.7.18): JSON parsing for configuration.
  - `grpc` (>= 1.54): gRPC for networking.
  - `ncurses` (>= 6.4): TUI support.
  - Optional: `protobuf`, `grpc-tools` (for regenerating Protobuf/gRPC files), `valgrind`, `gdb`, `cmocka`, `doxygen` (for R&D).
- **Tools**: `git`, `make`, `g++` or `clang++`, `pkgconf` (for `pkg-config`).

## Build Instructions

### Option 1: Docker (Recommended for Portability)
Use an Alpine Linux Docker container for a lightweight, reproducible build environment.

1. **Create `Dockerfile`**:
   ```dockerfile
   FROM alpine:edge
   RUN apk update && apk add --no-cache cmake make g++ protobuf-c-dev util-linux-dev grpc-dev ncurses-dev pkgconf git wget unzip linux-headers && rm -rf /var/cache/apk/*
   RUN wget https://github.com/DaveGamble/cJSON/archive/refs/tags/v1.7.18.zip && unzip v1.7.18.zip && cd cJSON-1.7.18 && mkdir build && cd build && cmake .. && make && make install && cd ../.. && rm -rf cJSON-1.7.18 v1.7.18.zip
   RUN git clone https://github.com/ALH477/DeMoD-Communication-Framework /dcf && cd /dcf/c_sdk
   WORKDIR /dcf/c_sdk
   RUN mkdir build && cd build && cmake .. && make
   ENTRYPOINT ["./build/dcf", "version", "--json"]
   ```

2. **Build and Run**:
   ```bash
   cd C_SDK
   docker build -t dcf-c-sdk .
   docker run --rm dcf-c-sdk
   ```
   Expected output:
   ```json
   {"version":"5.0.0"}
   ```

3. **Interactive Development**:
   ```bash
   docker run -it --rm -v $(pwd):/dcf/c_sdk dcf-c-sdk sh
   cd build
   cmake .. && make
   ./dcf tui
   ```

### Option 2: Nix Shell (For NixOS Users)
Use a Nix shell for a declarative, reproducible environment.

1. **Create `shell.nix`**:
   ```nix
   { pkgs ? import (fetchTarball {
       url = "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
       sha256 = "sha256:1s3lxb33cwazlx72pygcbcc76bbgbhdil6q9bhqbzbjxj001zk0w";
     }) {} }:

   pkgs.mkShell {
     buildInputs = [
       pkgs.cmake pkgs.protobufc pkgs.libuuid pkgs.cjson pkgs.grpc pkgs.ncurses
       pkgs.protobuf pkgs.grpc-tools pkgs.valgrind pkgs.gdb pkgs.strace pkgs.clang-tools pkgs.cmocka pkgs.doxygen
     ];
     shellHook = ''
       export CFLAGS="-I${pkgs.cjson}/include/cjson -I${pkgs.protobufc}/include -I${pkgs.libuuid}/include -I${pkgs.grpc}/include -I${pkgs.ncurses}/include"
       export LDFLAGS="-L${pkgs.cjson}/lib -L${pkgs.protobufc}/lib -L${pkgs.libuuid}/lib -L${pkgs.grpc}/lib -L${pkgs.ncurses}/lib"
       export CMAKE_PREFIX_PATH="${pkgs.protobufc}:${pkgs.cjson}:${pkgs.libuuid}:${pkgs.grpc}:${pkgs.ncurses}:${pkgs.protobuf}"
       export PKG_CONFIG_PATH="${pkgs.protobufc}/lib/pkgconfig:${pkgs.cjson}/lib/pkgconfig:${pkgs.libuuid}/lib/pkgconfig:${pkgs.grpc}/lib/pkgconfig:${pkgs.ncurses}/lib/pkgconfig:${pkgs.protobuf}/lib/pkgconfig"
       echo "Nix shell for DCF C SDK ready!"
     '';
   }
   ```

2. **Enter Shell and Build**:
   ```bash
   cd C_SDK
   nix-shell
   mkdir build && cd build
   cmake .. && make
   ../bin/dcf version --json
   ```

3. **Custom CMake Modules** (if needed):
   If `find_package` fails, add `cmake/FindProtobuf-c.cmake` and `cmake/FindUUID.cmake` (see [Nix Shell Guide](#troubleshooting)) and ensure `CMakeLists.txt` includes:
   ```cmake
   list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
   ```

### Option 3: Manual Build (Ubuntu/Debian)
1. **Install Dependencies**:
   ```bash
   sudo apt update
   sudo apt install -y cmake make g++ libprotobuf-c-dev uuid-dev libgrpc++-dev libncurses-dev pkg-config git
   wget https://github.com/DaveGamble/cJSON/archive/refs/tags/v1.7.18.tar.gz
   tar -xzf v1.7.18.tar.gz && cd cJSON-1.7.18
   mkdir build && cd build && cmake .. && make && sudo make install
   cd ../.. && rm -rf cJSON-1.7.18 v1.7.18.tar.gz
   ```

2. **Clone and Build**:
   ```bash
   git clone https://github.com/ALH477/DeMoD-Communication-Framework
   cd DeMoD-Communication-Framework/C_SDK
   mkdir build && cd build
   cmake .. && make
   ../bin/dcf version --json
   ```

## CLI Commands
The `dcf` binary provides a CLI for scripting and operation. All commands support `--json` for machine-readable output, ideal for scripting with tools like `jq` or Python.

- **dcf init [config_path]**: Initializes with a configuration file (default: `config.json`). Example: `dcf init config.json --json`
- **dcf start**: Starts the DCF instance. Example: `dcf start --json`
- **dcf stop**: Stops the DCF instance. Example: `dcf stop --json`
- **dcf status**: Displays status (running, mode, peers, RTT). Example: `dcf status --json`
- **dcf send [data] [recipient]**: Sends a message to a peer. Example: `dcf send "Hello" "peer1" --json`
- **dcf receive**: Receives a message. Example: `dcf receive --json`
- **dcf health-check [peer]**: Performs a health check on a peer, returning RTT. Example: `dcf health-check "peer1" --json`
- **dcf list-peers**: Lists peers with RTT and group ID. Example: `dcf list-peers --json`
- **dcf heal [peer]**: Initiates network healing for a peer. Example: `dcf heal "peer1" --json`
- **dcf version**: Displays SDK version. Example: `dcf version --json`
- **dcf benchmark [peer]**: Benchmarks a peer’s performance. Example: `dcf benchmark "peer1" --json`
- **dcf group-peers**: Regroups peers by RTT for AUTO mode. Example: `dcf group-peers --json`
- **dcf simulate-failure [peer]**: Simulates a peer failure for testing. Example: `dcf simulate-failure "peer1" --json`
- **dcf log-level [level]**: Sets log level (0=debug, 1=info, 2=error). Example: `dcf log-level 1 --json`
- **dcf load-plugin [path]**: Loads a plugin dynamically. Example: `dcf load-plugin "libcustom.so" --json`
- **dcf tui**: Launches the ncurses-based Text User Interface (TUI) for interactive monitoring.

### Example Config (`config.json`)
```json
{
  "mode": "auto",
  "node_id": "node1",
  "peers": ["peer1", "peer2"],
  "host": "localhost",
  "port": 50051,
  "rtt_threshold": 50,
  "plugins": "libcustom.so"
}
```

## Scripting
Use `--json` for machine-readable output:
```bash
dcf status --json | jq '.peer_count'
```
Chain commands for workflows:
```bash
dcf init config.json && dcf start && dcf send "Hello" "peer1" --json
```

## Text User Interface (TUI)
Run `dcf tui` to launch an ncurses-based interface for real-time monitoring of peers, RTT, and network status, with interactive command execution.

## R&D and Debugging
- **Memory Safety**: Test with Valgrind:
  ```bash
  valgrind --leak-check=full ./build/dcf status
  ```
- **Debugging**: Use GDB:
  ```bash
  gdb ./build/dcf
  ```
- **Unit Testing**: Extend `tests/test_redundancy.c` with `cmocka`:
  ```c
  #include <cmocka.h>
  void test_example(void **state) { assert_int_equal(1, 1); }
  int main() { const struct CMUnitTest tests[] = { cmocka_unit_test(test_example) }; return cmocka_run_group_tests(tests, NULL, NULL); }
  ```
- **Documentation**: Generate API docs with Doxygen:
  ```bash
  doxygen Doxyfile
  ```
- **Code Analysis**: Use `clang-tidy`:
  ```bash
  clang-tidy src/dcf_sdk/dcf_client.c -checks=* -- -I/usr/local/include/cjson -I/usr/include
  ```

## Troubleshooting
- **CMake Errors**: If `find_package(Protobuf-c)` or `find_package(UUID)` fails, ensure `pkgconf` is installed and `PKG_CONFIG_PATH` includes:
  ```bash
  export PKG_CONFIG_PATH=/usr/lib/pkgconfig:/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
  ```
  Alternatively, add `cmake/FindProtobuf-c.cmake` and `cmake/FindUUID.cmake` (see [Nix Shell Guide](#build-instructions)).
- **Missing cJSON**: If `cjson` is not found, verify `/usr/local/lib/libcjson.so` and `/usr/local/include/cjson/cjson.h` exist after building from source.
- **Protobuf Regeneration**: To update `proto/messages.pb-c.c`:
  ```bash
  protoc --c_out=. proto/messages.proto
  ```
- **Docker Issues**: If the Docker build fails, check network access for `git clone` or `wget`. Use `--no-cache` to rebuild:
  ```bash
  docker build --no-cache -t dcf-c-sdk .
  ```

## License
The C SDK is licensed under GPL-3.0. All dependencies (`protobuf-c`, `libuuid`, `cjson`, `grpc`, `ncurses`) are compatible open-source licenses.

## Contributing
Submit pull requests to [github.com/ALH477/DeMoD-Communication-Framework](https://github.com/ALH477/DeMoD-Communication-Framework). For issues, file a bug with logs and reproduction steps.
