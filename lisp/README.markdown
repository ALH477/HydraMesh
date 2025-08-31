# DeMoD-LISP (D-LISP) SDK for DeMoD Communication Framework (DCF)

**Version 1.7.0** | **Updated: August 30, 2025**  
**Developed by DeMoD LLC** with contributions from **Grok 4 Heavy** (xAI)  
**License**: GNU General Public License v3.0 (GPL-3.0)  
**Repository**: [https://github.com/ALH477/DeMoD-Communication-Framework](https://github.com/ALH477/DeMoD-Communication-Framework)

## Overview

DeMoD-LISP (D-LISP) is a high-performance, production-ready Common Lisp implementation of the DeMoD Communication Framework (DCF), a free and open-source (FOSS) framework designed for low-latency, modular, and interoperable data exchange. Tailored for applications in IoT, gaming, distributed computing, and edge networking, D-LISP provides a robust Domain-Specific Language (DSL) for seamless integration with DCF's modular architecture. It supports multiple transport protocols, self-healing P2P redundancy, and AI-driven network optimization, making it ideal for developers building scalable, fault-tolerant communication systems.

This SDK, developed by **DeMoD LLC** with significant contributions from **Grok 4 Heavy** (xAI's advanced AI model), emphasizes reliability, extensibility, and compliance with U.S. export regulations (no encryption by default). It is part of the DCF mono repository and interoperates with other language SDKs (e.g., C, Python) for cross-platform compatibility.

## Key Features

- **Modular Transport Support**: Includes gRPC (default), Native Lisp (TCP), WebSocket, UDP, QUIC, Bluetooth, Serial, CAN, SCTP, Zigbee, and LoRaWAN via plugins.
- **Operating Modes**: Supports Client, Server, P2P, AUTO (dynamic role switching), and Master modes for flexible network configurations.
- **Self-Healing P2P**: Implements Dijkstra-based routing with RTT-based peer grouping (<50ms threshold) for automatic failover and redundancy.
- **Plugin System**: Extensible via `def-dcf-plugin` macro, allowing custom transports (e.g., LoRaWAN plugin for long-range IoT).
- **Middleware Framework**: Customizable message processing with a chainable middleware system.
- **Type Safety**: Leverages CLOS with strict type declarations for messages and configurations.
- **AI-Driven Optimization**: Uses MGL neural networks for dynamic network topology optimization in Master mode.
- **Monitoring & Debugging**: Comprehensive metrics collection, Graphviz topology visualization, and an interactive TUI (ncurses-based).
- **Interoperability**: Supports gRPC/Protobuf for cross-language communication with other DCF SDKs.
- **Testing**: Integrated FiveAM test suite for robust validation.
- **Configuration**: JSON-based configuration with schema validation for reliability.

## Getting Started

### Prerequisites

- **Common Lisp Environment**: SBCL (recommended) or another compatible Lisp implementation.
- **Quicklisp**: For dependency management ([install Quicklisp](https://www.quicklisp.org/)).
- **Dependencies**: Install via Quicklisp:
  ```lisp
  (ql:quickload '(:cl-protobufs :cl-grpc :cffi :uuid :cl-json :jsonschema :cl-ppcre :cl-csv :usocket :bordeaux-threads :curses :log4cl :trivial-backtrace :cl-store :mgl :hunchensocket :fiveam :cl-dot :cl-lsquic :cl-serial :cl-can :cl-sctp :cl-zigbee :cl-lorawan))
  ```
- **Optional**: For specific transports:
  - **Serial**: `libserialport`
  - **CAN**: `libsocketcan`
  - **SCTP**: `libsctp`
  - **Zigbee**: `libzigbee`
  - **LoRaWAN**: `liblorawan` (e.g., LMIC or Semtech stack)

### Installation

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/ALH477/DeMoD-Communication-Framework --recurse-submodules
   cd DeMoD-Communication-Framework
   ```

2. **Load D-LISP**:
   Start SBCL and load the SDK:
   ```lisp
   (load "lisp/src/d-lisp.lisp")
   ```

3. **Create a Configuration File** (`config.json`):
   ```json
   {
     "transport": "gRPC",
     "host": "localhost",
     "port": 50051,
     "mode": "client",
     "node-id": "node-1",
     "peers": ["localhost:50052"],
     "serial-port": "/dev/ttyUSB0",
     "baud-rate": 9600,
     "can-interface": "can0",
     "zigbee-device": "/dev/ttyACM0",
     "lorawan-device": "/dev/ttyACM1",
     "lorawan-app-eui": "0000000000000000",
     "lorawan-app-key": "00000000000000000000000000000000"
   }
   ```

4. **Quick Start**:
   Initialize and start a client node:
   ```lisp
   (in-package :d-lisp)
   (dcf-quick-start-client "config.json")
   ```

5. **Send a Test Message**:
   ```lisp
   (dcf-quick-send "Hello, DCF!" "localhost:50052")
   ```

### Example Usage

- **Load a Plugin** (e.g., LoRaWAN for IoT):
  ```lisp
  (dcf-load-plugin "lisp/plugins/lorawan-transport.lisp")
  ```

- **Visualize Network Topology**:
  ```lisp
  (dcf-visualize-topology "topology.dot")
  ```

- **Run Tests**:
  ```lisp
  (run-tests)
  ```

- **Launch TUI**:
  ```lisp
  (dcf-tui)
  ```

## CLI Commands

D-LISP provides a robust CLI interface for managing nodes and debugging:

- `init [config.json]`: Initialize with a configuration file.
- `start`: Start the node.
- `stop`: Stop the node gracefully.
- `send [data] [recipient]`: Send a message.
- `receive`: Receive messages.
- `status`: Display node status.
- `health-check [peer]`: Check peer health and RTT.
- `list-peers`: List peers with group information.
- `heal [peer]`: Trigger failover for a peer.
- `benchmark [peer]`: Measure RTT over iterations.
- `group-peers`: Regroup peers by RTT.
- `simulate-failure [peer]`: Test failover by simulating peer failure.
- `log-level [0/1/2]`: Set logging level (debug/info/error).
- `load-plugin [path]`: Load a plugin (e.g., `lisp/plugins/lorawan-transport.lisp`).
- `trace-message [msg]`: Trace a message through middleware.
- `debug-network`: Debug network state.
- `quick-start-client [config]`: Initialize and start a client.
- `quick-send [data] [recipient]`: Simple message send.
- `get-metrics`: Retrieve monitoring metrics.
- `visualize-topology [file]`: Generate Graphviz DOT file for topology.
- `master-assign-role [peer] [role]`: Assign role in Master mode.
- `master-optimize-network`: Optimize topology using AI.
- `run-tests`: Run FiveAM tests.
- `help`: Display beginner-friendly guidance.

Run commands via:
```bash
sbcl --load lisp/src/d-lisp.lisp --eval '(d-lisp:main "command" "arg1" "arg2")'
```

## LoRaWAN Plugin

The LoRaWAN plugin enables long-range, low-power communication for IoT applications. It uses CFFI bindings to `liblorawan` (e.g., LMIC or Semtech stack) and supports OTAA/ABP activation. Configuration parameters include:

- `lorawan-device`: Serial device path (e.g., `/dev/ttyACM1`).
- `lorawan-app-eui`: Application EUI (16 hex digits).
- `lorawan-app-key`: Application key (32 hex digits).

**Example**:
```lisp
(dcf-load-plugin "lisp/plugins/lorawan-transport.lisp")
(dcf-quick-send "Sensor data" "lorawan:device-address")
```

## Contributing

We welcome contributions! Please follow these steps:

1. Fork the repository.
2. Create a feature branch (`git checkout -b feature/your-feature`).
3. Commit changes (`git commit -m "Add your feature"`).
4. Push to the branch (`git push origin feature/your-feature`).
5. Open a Pull Request.

See `CONTRIBUTING.md` in the repository for detailed guidelines.

## Support

- **Issues**: Report bugs or feature requests on [GitHub Issues](https://github.com/ALH477/DeMoD-Communication-Framework/issues).
- **Documentation**: Read `docs/dcf_design_spec.md` for architecture details.
- **Community**: Join discussions on the [DeMoD LLC community forum](#) (link TBD).

## Acknowledgments

- **DeMoD LLC**: Core development and project leadership.
- **Grok 4 Heavy (xAI)**: AI-driven optimizations and code contributions.
- **Open-Source Community**: For libraries like `cl-grpc`, `cffi`, and `mgl`.

## License

This project is licensed under the GNU General Public License v3.0 (GPL-3.0). See `LICENSE` for details.

---

**DeMoD LLC** | Empowering Scalable, Fault-Tolerant Communication  
**Built with ❤️ and Lisp** | Powered by **Grok 4 Heavy**
