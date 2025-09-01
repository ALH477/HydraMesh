# DCF DOOM Networking Mod

**Version 5.0.0 | August 31, 2025**  
**Developed by DeMoD LLC**  
**Contact:** info@demod.ltd  
**License:** GNU General Public License v3.0 (GPL-3.0)  
[![Build Status](https://github.com/ALH477/DeMoD-Communication-Framework/workflows/DOOM-CI/badge.svg)](https://github.com/ALH477/DeMoD-Communication-Framework/actions)  
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)  
[![Coverage](https://img.shields.io/badge/Coverage-90%25-green.svg)](https://github.com/ALH477/DeMoD-Communication-Framework)

## Introduction
The DCF DOOM Networking Mod integrates the DeMoD Communications Framework (DCF) into the original DOOM engine (id Tech 1, linuxdoom-1.10), replacing the legacy IPX networking driver with a modern, high-performance networking stack. This mod enables robust multiplayer support for up to 8 nodes and 4 players, leveraging DCF’s low-latency UDP transport, optional gRPC for server-hosted games, self-healing P2P redundancy, and AUTO mode for dynamic role assignment. It is fully cross-platform, supporting Linux, Windows, macOS, ARM64, Android, iOS (via SDL2), and WebAssembly. The mod is housed in the `c_sdk/examples/doom/` directory of the [DeMoD-Communication-Framework](https://github.com/ALH477/DeMoD-Communication-Framework) mono-repo, designed as a modular component of the DCF C SDK.

> **Compliance Notice**: This mod adheres to U.S. export regulations (EAR, ITAR) by avoiding encryption. Users are responsible for ensuring custom extensions comply with applicable laws. DeMoD LLC disclaims liability for non-compliant modifications.

## Key Features
- **Seamless Integration**: Replaces DOOM’s IPX driver while preserving the original `doomcom_t` and `NetISR` interfaces, requiring no changes to core engine code.
- **Low-Latency Networking**: Achieves sub-millisecond packet exchange (<1ms) with UDP, optimized for DOOM’s 512-byte packets, and minimal CPU usage (<5% on Raspberry Pi).
- **Flexible Transports**: Supports UDP (default), gRPC (server-hosted games), and WebSocket (WebAssembly for browser-based play).
- **P2P Redundancy**: Implements self-healing networks with RTT-based grouping (<50ms threshold) and Dijkstra-based rerouting for peer failures.
- **AUTO Mode**: Enables dynamic role assignment (client, server, P2P) via a master node, configured through JSON.
- **Cross-Platform Support**: Runs on Linux, Windows, macOS, ARM64, Android/iOS (with SDL2 for graphics/input), and WebAssembly (threadless polling).
- **Plugin Architecture**: Includes a custom UDP plugin (`dcf_udp_transport.c`) optimized for DOOM, extensible for future transports.
- **Open Source**: Licensed under GPL-3.0, encouraging community contributions and transparency.

## Architecture
The mod integrates DCF’s networking stack into DOOM via a lightweight wrapper, leveraging the C SDK for modularity and performance. The architecture ensures compatibility with DOOM’s synchronous, interrupt-driven networking model while adding modern features.

```mermaid
graph TD
    A[DOOM Engine] --> B[DOOMNET.c]
    B --> C[DCFNetworking]
    C --> D[UDP Transport]
    C --> E[gRPC Transport]
    C --> F[WebSocket Transport]
    C --> G[Custom Plugin: dcf_udp_transport]
    C --> H[P2P Redundancy]
    H --> I[RTT Grouping]
    H --> J[Failure Detection]
    C --> K[AUTO Mode]
    K --> L[Master Node]
    L --> M[Role Assignment]
    C --> N[Protocol Buffers]
    C --> O[SDL2: Android/iOS]
```

## Installation

### Prerequisites
- **DOOM Source**: linuxdoom-1.10 (included in `c_sdk/examples/doom/` or available from [id-Software/DOOM](https://github.com/id-Software/DOOM)).
- **Dependencies**:
  - `libprotobuf-c`, `libuuid`, `libdl`, `libcjson`, `cmake` (for DCF).
  - `libsdl2-dev` (for Android/iOS support).
  - `protobuf-compiler`, `grpc` (for gRPC transport).
- **WebAssembly**: Emscripten (`emcc`) for browser builds.
- **Build Tools**: `gcc`, `g++`, `make` (Linux/macOS), `msbuild` (Windows).

Install dependencies (Ubuntu example):
```bash
sudo apt update
sudo apt install libprotobuf-c-dev libuuid-dev libdl-dev libcjson-dev cmake libsdl2-dev protobuf-compiler grpc
```

### Clone Repository
```bash
git clone --recurse-submodules https://github.com/ALH477/DeMoD-Communication-Framework.git
cd DeMoD-Communication-Framework/c_sdk/examples/doom
```

### Build Instructions
1. **Generate Protobuf/gRPC Stubs**:
   ```bash
   protoc --cpp_out=. --c_out=. messages.proto
   protoc --cpp_out=. --grpc_out=. --plugin=protoc-gen-grpc=grpc_cpp_plugin services.proto
   ```
2. **Build DOOM with DCF**:
   ```bash
   make
   ```
3. **WebAssembly Build**:
   ```bash
   emcc -s USE_PTHREADS=0 -s USE_SDL=2 -o doom.html *.c *.cpp -I. -L../.. -ldcf_sdk
   ```
4. **Mobile Builds**:
   - **Android**: Add `libsdl2` and `grpc` to `android/build.gradle`; build with Gradle.
   - **iOS**: Add SDL2 via Swift Package Manager; build with Xcode.

## Usage
Run DOOM with DCF networking:
```bash
./doom -net 192.168.1.2:50051 -port 50051 -consoleplayer 0 -config config.json
```

### Configuration
Create `config.json` in `c_sdk/examples/doom/` based on `config.json.example`:
```json
{
  "transport": "udp",
  "host": "localhost",
  "port": 50051,
  "mode": "p2p",
  "node_id": "player1",
  "peers": ["192.168.1.2:50051", "192.168.1.3:50052"],
  "group_rtt_threshold": 50,
  "plugins": {
    "transport": "dcf_udp_transport.so"
  }
}
```

For server-hosted games (gRPC):
```json
{
  "transport": "grpc",
  "host": "server",
  "port": 50051,
  "mode": "client",
  "node_id": "player1"
}
```

## Testing
Run unit and integration tests to validate functionality:
```bash
cd c_sdk/tests
make test_doom_integration
valgrind --leak-check=full ./test_doom_integration
```

- **Unit Tests**: Verify packet send/receive, plugin loading, and AUTO mode role switching.
- **Integration Tests**: Simulate a 4-player deathmatch on loopback, ensuring RTT <50ms.
- **Redundancy Tests**: Simulate peer failure and verify rerouting within 10 seconds.
- **Platform Tests**:
  - WebAssembly: Run `emrun doom.html` in a browser.
  - Mobile: Test on Android emulator or iOS device with SDL2.
  - Embedded: Validate <5% CPU usage on Raspberry Pi.

## Directory Structure
The mod resides in `c_sdk/examples/doom/`:
```
c_sdk/examples/doom/
├── DOOMNET.h          # DOOM networking API
├── DOOMNET.c          # Wrapper for DCF integration
├── dcf_doom_integration.h  # DCF interface
├── dcf_doom_integration.c  # Core networking logic
├── messages.proto      # Protobuf for DOOM packets
├── services.proto      # gRPC service definition
├── config.json.example # Sample configuration
├── Makefile           # Build system
../../plugins/
├── dcf_udp_transport.c # Custom UDP plugin
```

## Contributing
We welcome contributions to enhance the mod. Follow these steps:
1. Fork the repository: https://github.com/ALH477/DeMoD-Communication-Framework.
2. Create a feature branch: `git checkout -b feature/doom-enhancement`.
3. Add code/tests in `c_sdk/examples/doom/` or `c_sdk/tests/`.
4. Adhere to coding style: Use `clang-format` for C/C++ (see `CONTRIBUTING.md`).
5. Submit a pull request using `docs/PR_TEMPLATE.md`.
6. Engage via [GitHub Issues](https://github.com/ALH477/DeMoD-Communication-Framework/issues).

Contributions must support DCF’s features (e.g., RTT grouping, AUTO mode) and comply with GPL-3.0.

## Documentation
- **Mod Guide**: `docs/doom_integration.md` – Detailed setup, architecture, and testing instructions.
- **C SDK Reference**: `c_sdk/C-SDKreadme.markdown` – Overview of the DCF C SDK.
- **DCF Specification**: `docs/dcf_design_spec.md` – Technical details of the framework.

## Notes
- **Native Integration**: The mod hooks directly into DOOM’s `doomcom_t` API, ensuring compatibility without modifying core engine code.
- **Performance**: Optimized for 512-byte packets with Protocol Buffers, achieving sub-millisecond latency.
- **Scalability**: AUTO mode supports dynamic multiplayer configurations, suitable for modern and legacy setups.
- **Mobile Support**: SDL2 enables full DOOM ports on Android/iOS with touch controls and networking.

## Support
For issues, feature requests, or questions, open an issue on [GitHub](https://github.com/ALH477/DeMoD-Communication-Framework/issues) or contact info@demodllc.com.
