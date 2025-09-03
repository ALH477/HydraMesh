# DeMoD Communications Framework (DCF)

**Version 5.0.0** | **September 2, 2025**  
**Developed by DeMoD LLC**  
**License**: GNU General Public License v3.0 (GPL-3.0)  

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)  
[Website](https://demod.ltd/dcf.html)

## Overview
The DeMoD Communications Framework (DCF) is a free and open-source (FOSS) framework evolved from the DeMoD Secure Protocol, designed for low-latency, modular, and interoperable data exchange. It supports applications such as IoT messaging, real-time gaming synchronization, distributed computing, and edge networking. DCF features a handshakeless design, efficient Protocol Buffers (Protobuf) serialization, and a compatibility layer for UDP, TCP, WebSocket, gRPC, and custom transports (e.g., LoRaWAN), enabling seamless peer-to-peer (P2P) networking with self-healing redundancy.

DCF is hardware- and language-agnostic, supporting embedded devices (e.g., Raspberry Pi), cloud servers, and mobile platforms (Android/iOS) with bindings in C, Lisp, Perl, Python, C++, JavaScript (Node.js), Go, Rust, Java/Kotlin, and Swift. Version 5.0.0 introduces a refined plugin system with separate registries for transport (singleton) and hardware (multi, keyed by ID), version checking (>=1.1), and JSON-based configuration, alongside an AUTO mode for dynamic role assignment managed by a master node. Licensed under GPL-3.0, DCF ensures open-source derivatives. It includes command-line interface (CLI), text user interface (TUI), server/client logic, P2P, and AUTO modes, making it versatile for standalone tools, libraries, or networked services.

> **Important**: DCF complies with U.S. Export Administration Regulations (EAR) and International Traffic in Arms Regulations (ITAR) by excluding encryption, ensuring export-control-free distribution. Users implementing custom extensions (e.g., TLS via plugins) must ensure compliance with applicable regulations. DeMoD LLC disclaims liability for non-compliant modifications.

## Features
- **Modularity**: Independent components with standardized APIs; plugin system with version checking, JSON parameters, and separate transport/hardware registries.
- **Interoperability**: Protobuf and gRPC ensure cross-language (C, Lisp, Perl, Python, C++, JavaScript, Go, Rust, Java, Swift) and cross-platform compatibility.
- **Low Latency**: Sub-millisecond exchanges (<1ms locally) with handshakeless design for real-time applications.
- **Flexibility**: Compatibility layer for UDP, TCP, WebSocket, gRPC, and custom transports (e.g., LoRaWAN); supports mobile bindings.
- **Dynamic Role Assignment**: AUTO mode enables nodes to switch between client, server, or P2P roles under master node control, supporting AI-driven network optimization.
- **Usability**: CLI for automation, TUI for monitoring; supports server, client, P2P, and AUTO modes with logging; master node commands for role and configuration management.
- **Self-Healing P2P**: Redundant paths (2-3 backups), failure detection within 10s, RTT-based grouping (<50ms threshold), and optimized routing (Dijkstra with RTT weights).
- **Open Source**: GPL-3.0 ensures transparency and encourages community contributions.

## Architecture
```mermaid
graph TD
    A[DCF Framework] --> B[CLI]
    A --> C[TUI]
    A --> D[Networking Layer]
    D --> E[gRPC (Default)]
    D --> F[UDP/TCP/WebSocket]
    D --> G[Custom Plugins (e.g., LoRaWAN)]
    A --> H[Redundancy Module]
    H --> I[RTT Grouping]
    H --> J[Failover Routing]
    A --> K[Plugin Manager]
    K --> L[Transport Registry (Singleton)]
    K --> M[Hardware Registry (Multi-ID)]
    A --> N[Configuration (JSON)]
    A --> O[Serialization (Protobuf)]
    A --> P[Master/AUTO Mode]
```

## Quick Start
1. Clone the repository: `git clone --recurse-submodules https://github.com/ALH477/DeMoD-Communication-Framework.git`.
2. Build an SDK (e.g., C SDK): `cd c_sdk && mkdir build && cd build && cmake .. && make`.
3. Run: `./dcf init config.json && ./dcf start`.
4. Send a message: `./dcf send "Hello" "peer1"`.

## SDKs and Bindings
- **C SDK**: Low-level, memory-safe implementation with refined plugin manager (`c_sdk/`).
- **Lisp SDK**: Expressive DSL with CLOS-based plugin manager for dynamic loading (`lisp/`).
- **Perl/Python SDKs**: High-level scripting support (`perl/`, `python/`).
- **Others**: C++, JavaScript (Node.js), Go, Rust in development.
- **Mobile**: Android (Java/Kotlin), iOS (Swift) bindings.

## Examples
### C Client (gRPC)
```c
#include <dcf_sdk/dcf_client.h>
int main() {
    DCFClient* client = dcf_client_new("localhost", 50051);
    char* response;
    dcf_client_send_message(client, "Hello", "peer1", &response);
    dcf_client_free(client);
    return 0;
}
```

### Lisp Client
```lisp
(dcf-quick-start-client "config.json")
(dcf-quick-send "Hello, DCF!" "peer1")
```

### Plugin Example (C Transport for C SDK)
```c
#include <dcf_sdk/dcf_plugin_manager.h>
typedef struct { /* Private data */ } CustomTransport;
bool setup(void* self, const char* params, int timeout_ms) { return true; }
bool send(void* self, const uint8_t* data, size_t size, const char* target) { return true; }
uint8_t* receive(void* self, size_t* size) { *size = 0; return NULL; }
bool health_check(void* self, const char* target, int* rtt_ms) { *rtt_ms = 10; return true; }
void destroy(void* self) { free(self); }
ITransport iface = {setup, send, receive, health_check, destroy};
void* create_plugin() { return calloc(1, sizeof(CustomTransport)); }
const char* get_plugin_version() { return "1.1.0"; }
```

## Configuration
Create `config.json` based on `config.json.example`:
```json
{
  "transport": "gRPC",
  "host": "localhost",
  "port": 50051,
  "mode": "auto",
  "node_id": "node1",
  "peers": ["localhost:50051", "localhost:50052"],
  "group_rtt_threshold": 50,
  "plugins": {
    "transport": "libcustom_transport.so",
    "hardware": {"i2c_sensor": "libi2c_sensor.so", "usb_device": "libusb_device.so"}
  },
  "transport-params": {},
  "hardware-params": {"i2c_sensor": {}, "usb_device": {}}
}
```

For master node:
```json
{
  "transport": "gRPC",
  "host": "localhost",
  "port": 50051,
  "mode": "master",
  "node_id": "master1",
  "peers": ["localhost:50052", "localhost:50053"],
  "group_rtt_threshold": 50
}
```

## Testing
Run tests with:
- **C SDK**: `cd c_sdk/build && make test_redundancy test_plugin && valgrind --leak-check=full ./p2p`.
- **Lisp SDK**: `(ql:quickload :d-lisp) (d-lisp:run-tests)` with FiveAM.
- **Perl**: `prove -r tests/`.
- **Python**: `pytest tests/`.
- **Others**: To be implemented (e.g., `cargo test` for Rust).
- **Integration**: Use Docker for multi-language setups; test RTT grouping, failover, AUTO mode role assignment, and plugin loading.

## Contributing
Contributions are welcome! Follow these steps:
1. Fork the repository.
2. Create a feature branch (`git checkout -b feature/xyz`).
3. Add tests and code, adhering to style guidelines (`perltidy` for Perl, `black` for Python, `clang-format` for C/C++, `lispindent` for Lisp).
4. Submit a pull request with a clear description using `docs/PR_TEMPLATE.md`.
5. Discuss via [GitHub Issues](https://github.com/ALH477/DeMoD-Communication-Framework/issues).

New SDKs are encouraged; ensure support for RTT grouping, refined plugin manager, AUTO mode, and GPL-3.0 compliance.

## Documentation
- Architecture, SDK guidelines, plugin system, and testing: `docs/dcf_design_spec.md`.
- C SDK: `c_sdk/README.md`.
- Lisp SDK: `lisp/README.md`.

## License
Licensed under GPL-3.0, ensuring source availability, no warranty, and copyleft for derivatives. See `LICENSE` for details.