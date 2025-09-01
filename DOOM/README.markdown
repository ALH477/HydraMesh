# DCF DOOM Networking Mod

**Version 5.0.0 | August 31, 2025**  
**Developed by DeMoD LLC**  
**Contact:** info@demodllc.example  
**License:** GNU General Public License v3.0 (GPL-3.0)  
[![Build Status](https://github.com/ALH477/DeMoD-Communication-Framework/workflows/DOOM-CI/badge.svg)](https://github.com/ALH477/DeMoD-Communication-Framework/actions)  
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)  
[![Coverage](https://img.shields.io/badge/Coverage-90%25-green.svg)](https://github.com/ALH477/DeMoD-Communication-Framework)

## Overview
The DCF DOOM Networking Mod integrates the DeMoD Communications Framework (DCF) into the original DOOM engine (id Tech 1, based on linuxdoom-1.10), replacing the legacy IPX networking driver with a modern, low-latency, and modular alternative. This mod enables seamless multiplayer for up to 8 nodes and 4 players, leveraging DCF’s handshakeless UDP transport, optional gRPC for server-hosted games, self-healing P2P redundancy, and AUTO mode for dynamic role assignment. It supports cross-platform deployment, including embedded devices (e.g., Raspberry Pi), desktops (Linux, Windows, macOS), mobile (Android, iOS via SDL2), and browsers (WebAssembly). The mod is housed in the `c_sdk/examples/doom/` directory of the [DeMoD-Communication-Framework](https://github.com/ALH477/DeMoD-Communication-Framework) mono-repo, aligning with the C SDK’s modular design.

> **Important**: This mod complies with U.S. export regulations (EAR, ITAR) by avoiding encryption, per DCF’s design. Users must ensure custom extensions comply; consult legal experts for specific use cases. DeMoD LLC disclaims liability for non-compliant modifications.

## Features
- **Native Integration**: Replaces DOOM’s IPX driver (`DOOMNET.C`) without altering core engine code, using original `doomcom_t` and `NetISR` interfaces.
- **Low Latency**: Sub-millisecond packet exchange via UDP (<1ms, <5% CPU on Raspberry Pi), optimized for DOOM’s 512-byte packets.
- **Transport Options**: Default UDP for low latency; optional gRPC for server-hosted games; WebSocket for browser-based play (WebAssembly).
- **P2P Redundancy**: Self-healing network with RTT-based grouping (<50ms clusters) and Dijkstra-based rerouting on peer failure.
- **AUTO Mode**: Dynamic role assignment (client, server, P2P) via master node, with JSON-based configuration and metrics.
- **Cross-Platform**: Supports Linux, Windows, macOS, ARM64, Android/iOS (with SDL2 for graphics/input), and WebAssembly (threadless polling).
- **Plugin System**: Custom UDP transport plugin (`dcf_udp_transport.c`) for optimized DOOM packets; extensible for future transports.
- **Usability**: CLI for scripting (e.g., `dcf send "Fire" "peer1"`), TUI for real-time monitoring, JSON config for flexibility.
- **Open Source**: GPL-3.0 ensures transparency and community contributions.

## Architecture
The mod hooks into DOOM’s networking via `DOOMNET.h`, delegating to DCF’s C SDK for transport, serialization (Protocol Buffers), and redundancy. It supports all DOOM multiplayer modes (e.g., deathmatch) and extends to modern platforms.

```mermaid
graph TD
    A[DOOM Engine] --> B[DOOMNET.C]
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
Clone the mono-repo with submodules:
```bash
git clone --recurse-submodules https://github.com/ALH477/DeMoD-Communication-Framework.git
cd DeMoD-Communication-Framework/c_sdk/examples/doom
```

### Prerequisites
- **DOOM Source**: linuxdoom-1.10 (included in `c_sdk/examples/doom` or from id Software’s [DOOM repo](https://github.com/id-Software/DOOM)).
- **C SDK Dependencies**: `libprotobuf-c`, `libuuid`, `libdl`, `libcjson`, `cmake`, `ncurses` (per `DCF-README.markdown`).
- **Protobuf/gRPC**: `protobuf-compiler`, `grpc`, `grpc_cpp_plugin`.
- **SDL2 (Mobile)**: `libsdl2` for Android/iOS graphics/input.
- **WebAssembly**: Emscripten (`emcc`) for browser builds.
- **Build Tools**: `gcc`, `g++`, `make` (Linux/macOS), `msbuild` (Windows).

Install dependencies (Ubuntu example):
```bash
sudo apt install libprotobuf-c-dev libuuid-dev libdl-dev libcjson-dev cmake libncurses-dev libsdl2-dev protobuf-compiler grpc
```

### Build Steps
1. **Generate Protobuf/gRPC Stubs**:
   ```bash
   protoc --cpp_out=. --grpc_out=. --plugin=protoc-gen-grpc=grpc_cpp_plugin messages.proto services.proto
   protoc --c_out=. messages.proto
   ```
2. **Build DOOM with DCF**:
   ```bash
   cd c_sdk/examples/doom
   make
   ```
3. **WebAssembly Build**:
   ```bash
   emcc -s USE_PTHREADS=0 -s USE_SDL=2 -o doom.html *.c *.cpp -I. -L../.. -ldcf_sdk
   ```
4. **Mobile Build**:
   - Android: Update `android/build.gradle` with SDL2 and gRPC dependencies; build with Gradle.
   - iOS: Add SDL2 via Swift Package Manager; build with Xcode.

## Usage
Run DOOM with DCF networking:
```bash
./doom -net 192.168.1.2:50051 -port 50051 -consoleplayer 0 -config config.json
```

### Configuration
Create `config.json` in `c_sdk/examples/doom/` (based on `config.json.example`):
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

### CLI Commands
Use the C SDK’s CLI (`dcf`) for scripting and debugging:
- `dcf init config.json`: Initialize with config.
- `dcf start`: Start networking.
- `dcf send "Fire" "player2"`: Send a game packet.
- `dcf receive`: Receive a packet.
- `dcf health-check "192.168.1.2:50051" --json`: Check peer RTT.
- `dcf tui`: Launch interactive TUI for monitoring peers and metrics.

Example:
```bash
dcf init config.json && dcf start && dcf send "Fire" "player2" --json | jq '.status'
```

### TUI
Run `dcf tui` for a real-time ncurses-based interface to monitor peers, RTT, and network status.

## Testing
Run tests to validate integration:
```bash
cd c_sdk/tests
make test_doom_integration
valgrind --leak-check=full ./test_doom_integration
```
- **Unit Tests**: Cover packet send/receive, plugin loading, AUTO mode role switching.
- **Integration Tests**: Simulate 4-player deathmatch on loopback; verify RTT <50ms.
- **Redundancy Tests**: Run `dcf simulate-failure "player2"`; confirm rerouting within 10s.
- **Platform Tests**:
  - WebAssembly: Test in browser with `emrun doom.html`.
  - Mobile: Test on Android emulator or iOS device with SDL2.
  - Embedded: Test on Raspberry Pi, verify <5% CPU usage.

## Contributing
Contributions are welcome! Follow these steps:
1. Fork the repo: `https://github.com/ALH477/DeMoD-Communication-Framework`.
2. Create a feature branch: `git checkout -b feature/doom-enhancement`.
3. Add code/tests in `c_sdk/examples/doom/` or `c_sdk/tests/`.
4. Follow style: `clang-format` for C/C++ (per `CONTRIBUTING.md`).
5. Submit a PR using `docs/PR_TEMPLATE.md`.
6. Discuss via [GitHub Issues](https://github.com/ALH477/DeMoD-Communication-Framework/issues).

New features (e.g., new plugins, gRPC extensions) must support RTT grouping, AUTO mode, and GPL-3.0 compliance.

## Documentation
- **Detailed Guide**: `docs/doom_integration.md` for architecture, setup, and testing.
- **C SDK Docs**: `c_sdk/C-SDKreadme.markdown` for CLI/TUI details.
- **DCF Spec**: `docs/dcf_design_spec.md` for framework overview.

## Notes
- The mod preserves DOOM’s simplicity, hooking into `doomcom_t` without core engine changes.
- Optimized for DOOM’s 512-byte packets, with Protobuf serialization for efficiency.
- AUTO mode enables scalable multiplayer with master node control (e.g., for 8+ nodes in modern setups).
- SDL2 ensures mobile ports work seamlessly (e.g., touch controls for Android/iOS).
