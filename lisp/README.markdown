# DeMoD-LISP (D-LISP) SDK

**Version 1.7.0 | August 30, 2025**  
**Developed by DeMoD LLC with contributions from Grok 4**  
**Contact:** info@demod.ltd  
**License:** GNU General Public License v3.0 (GPL-3.0)  
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)  

## Overview
The DeMoD-LISP (D-LISP) SDK is a Common Lisp-based implementation of the DeMoD Communications Framework (DCF), an open-source, modular framework for low-latency data exchange in applications such as IoT messaging, real-time gaming synchronization, distributed computing, and edge networking. D-LISP leverages Lisp's expressive S-expression syntax, macro system, and dynamic typing to provide a flexible, extensible, and high-performance interface for DCF's networking capabilities.

D-LISP is hardware and language agnostic, supporting embedded devices (e.g., Raspberry Pi), cloud servers, and mobile platforms. It is fully interoperable with other DCF SDKs (e.g., C, Python) via Protocol Buffers and gRPC. Version 1.7.0 introduces support for additional transport plugins, including LoRaWAN for long-range, low-power IoT communication.

Developed by DeMoD LLC with heavy contributions from Grok 4, D-LISP is licensed under GPL-3.0, ensuring open-source derivatives. It includes CLI, TUI, middleware, type safety, connection pooling, metrics, and visualization tools, making it versatile for standalone tools, libraries, or networked services.

> **Important**: D-LISP complies with U.S. export regulations (EAR and ITAR). It avoids encryption to remain export-control-free. Users must ensure custom extensions comply; consult legal experts for specific use cases. DeMoD LLC disclaims liability for non-compliant modifications.

## Features
- **Modularity**: Independent components with standardized APIs; plugin system for custom transports (e.g., UDP, QUIC, Bluetooth, Serial, CAN, SCTP, Zigbee, LoRaWAN).
- **Interoperability**: Protocol Buffers and gRPC ensure cross-language compatibility with Perl, Python, C, C++, JS, Go, Rust, Java, Swift.
- **Low Latency**: Sub-millisecond exchanges with handshakeless design; supports multiple transports for real-time applications.
- **Flexibility**: Compatibility layer for gRPC, native Lisp, WebSocket, and custom plugins; middleware for protocol customization.
- **Dynamic Role Assignment**: AUTO mode for role switching under master node control, with AI-driven optimization using MGL.
- **Self-Healing P2P**: RTT-based grouping (<50ms clusters), Dijkstra routing, and failover.
- **Type Safety**: CLOS-based messages with type declarations and validation.
- **Diagnostics**: Middleware tracing, network debugging, metrics monitoring, and Graphviz topology visualization.
- **Usability**: CLI for automation, TUI for monitoring; facade API for simple use cases; FiveAM testing.
- **Open Source**: GPL-3.0 ensures transparency and community contributions.

## Architecture
```mermaid
graph TD
    A[D-LISP SDK] --> B[CLI]
    A --> C[TUI]
    A --> D[Networking Layer]
    
    D --> E[Server Mode]
    D --> F[Client Mode]
    D --> G[P2P Mode]
    D --> H[AUTO Mode]
    H --> I[Master Node]
    I --> J[Role Assignment]
    I --> K[Config Management]
    I --> L[Metrics Collection]
    L --> M[AI Optimization (MGL)]
    G --> N[Self-Healing Redundancy]
    N --> O[Peer Discovery]
    N --> P[Failure Detection]
    N --> Q[RTT-Based Grouping]
    
    D --> R[Transport Layer]
    R --> S[gRPC]
    R --> T[Native Lisp (USocket)]
    R --> U[WebSocket (Hunchensocket)]
    R --> V[UDP]
    R --> W[QUIC (cl-lsquic)]
    R --> X[Bluetooth]
    R --> Y[Serial]
    R --> Z[CAN]
    R --> AA[SCTP]
    R --> AB[Zigbee]
    R --> AC[LoRaWAN]
    R --> AD[Custom Plugins]
    
    D --> AE[Middleware Chain]
    AE --> AF[Protocol Customization]
    
    D --> AG[Protocol Buffers]
    AG --> AH[Serialization/Deserialization]
    
    A --> AI[Language Bindings]
    AI --> AJ[Common Lisp (SBCL)]
    
    A --> AK[Platform Support]
    AK --> AL[Embedded Devices (RPi)]
    AK --> AM[Cloud Servers]
    AK --> AN[Mobile (via Plugins)]
    
    A --> AO[Tools]
    AO --> AP[CLI/TUI]
    AO --> AQ[Facade API]
    AO --> AR[Diagnostics (Trace, Debug)]
    AO --> AS[Metrics & Monitoring]
    AO --> AT[Visualization (Graphviz)]
    AO --> AU[FiveAM Testing]

Installation
Clone the repository with submodules:
git clone --recurse-submodules https://github.com/ALH477/DeMoD-Communication-Framework.git
cd DeMoD-Communication-Framework/lisp

Prerequisites

Common Lisp: SBCL (recommended).
Quicklisp: For dependency management.
Libraries: cl-protobufs, cl-grpc, cffi, uuid, cl-json, jsonschema, cl-ppcre, cl-csv, usocket, bordeaux-threads, curses, log4cl, trivial-backtrace, cl-store, mgl, hunchensocket, fiveam, cl-dot, cl-lsquic, cl-serial, cl-can, cl-sctp, cl-zigbee, cl-lorawan.

Build Steps

Generate Protobuf/gRPC Stubs:protoc --lisp_out=lisp/src proto/messages.proto proto/services.proto


Load Dependencies:(ql:quickload :d-lisp)


Build ASDF System:(asdf:load-system :d-lisp)



Usage Examples
Quick Start Client
(dcf-quick-start-client "config.json")
(dcf-quick-send "Hello, DCF!" "localhost:50052")

Load Plugin (e.g., LoRaWAN)
(dcf-load-plugin "lisp/plugins/lorawan-transport.lisp")
(dcf-set-mode "p2p")
(dcf-start)

Send Message with Middleware
(add-middleware (lambda (msg dir) (format t "Processing ~A~%" dir) msg))
(dcf-send "Test" "recipient")

Visualize Topology
(dcf-visualize-topology "network.dot")

Run Tests
(run-tests)

Configuration
Create config.json based on config.json.example:
{
  "transport": "gRPC",
  "host": "localhost",
  "port": 50051,
  "mode": "client",
  "node-id": "node1",
  "peers": ["localhost:50052"],
  "group-rtt-threshold": 50,
  "plugins": {},
  "serial-port": "/dev/ttyUSB0",
  "baud-rate": 9600,
  "can-interface": "can0",
  "zigbee-device": "/dev/ttyACM0",
  "lorawan-device": "/dev/ttyACM1",
  "lorawan-app-eui": "0000000000000000",
  "lorawan-app-key": "00000000000000000000000000000000"
}

For LoRaWAN:
{
  "transport": "lorawan",
  "lorawan-device": "/dev/ttyACM1",
  "lorawan-app-eui": "0000000000000000",
  "lorawan-app-key": "00000000000000000000000000000000"
}

Testing
Run tests with:

Lisp: (asdf:test-system :d-lisp)
CLI: sbcl --eval "(asdf:test-system :d-lisp)" --quit

Contributing
Contributions are welcome! Follow these steps:

Fork the repo.
Create a feature branch (git checkout -b feature/xyz).
Add tests and code (follow Lisp style guidelines).
Submit a PR with a clear description.
Discuss issues via GitHub Issues.

New plugins and transports are encouraged; ensure GPL-3.0 compliance and FiveAM tests.
Documentation
See docs/dcf_design_spec.md for detailed architecture, SDK guidelines, plugin system, AUTO mode, and testing.
Developed by DeMoD LLC with heavy contributions from Grok 4.```
