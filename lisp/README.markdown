# DeMoD-LISP (D-LISP) SDK for DeMoD Communication Framework (DCF)

**Version 2.0.0** | **Updated: October 26, 2025**  
**Developed by DeMoD LLC**  
**License**: GNU Lesser General Public License v3.0 (LGPL-3.0)  
**Repository**: [https://github.com/ALH477/DeMoD-Communication-Framework](https://github.com/ALH477/DeMoD-Communication-Framework)  
**Demo (soon)**: [YouTube Video](https://youtu.be/IXbIB8jCvDE)

## Overview

DeMoD-LISP (D-LISP) is a high-performance, production-ready Common Lisp implementation of the DeMoD Communication Framework (DCF), a free and open-source (FOSS) framework designed for low-latency, modular, and interoperable data exchange. Tailored for applications in IoT, gaming, distributed computing, and edge networking, D-LISP provides a robust Domain-Specific Language (DSL) for seamless integration with DCF's modular architecture. It supports multiple transport protocols, self-healing P2P redundancy, AI-driven network optimization, and integrates **StreamDB** for persistent storage, making it ideal for developers building scalable, fault-tolerant communication systems.

This SDK, developed by **DeMoD LLC** with significant contributions from **Grok 4 Heavy** (xAI's advanced AI model), emphasizes reliability, extensibility, and compliance with U.S. export regulations (no encryption by default). It is part of the DCF mono repository and interoperates with other language SDKs (e.g., C, Python) for cross-platform compatibility.

### Efficiency Highlight: ~800 Lines of Code
D-LISP achieves its extensive functionality in approximately **800 non-comment, non-blank lines of code** (verified across the core implementation and plugins). This remarkable efficiency is made possible by Common Lisp's expressive features, such as macros (e.g., `def-dcf-plugin` for concise plugin definitions) and CLOS for type-safe abstractions. The enhanced integration of StreamDB adds powerful persistence with minimal overhead, maintaining a lean codebase while delivering a full SDK/DSL. This compactness ensures maintainability, reduces deployment overhead, and highlights Lisp's power for building complex systems with minimal verbosity.

- **Core Breakdown**:
  - `d-lisp.lisp` (main SDK/DSL): ~800 lines
  - Plugins (e.g., LoRaWAN, Serial, CAN, SCTP, Zigbee): ~150 lines total (~21-26 lines each)

## Key Features

- **Modular Transport Support**: Includes gRPC (default), Native Lisp (TCP), WebSocket, UDP, QUIC, Bluetooth, Serial, CAN, SCTP, Zigbee, and LoRaWAN via plugins.
- **Operating Modes**: Supports Client, Server, P2P, AUTO (dynamic role switching), and Master modes for flexible network configurations.
- **Self-Healing P2P**: Implements Dijkstra-based routing with RTT-based peer grouping (<50ms threshold) for automatic failover and redundancy.
- **Plugin System**: Extensible via `def-dcf-plugin` macro, allowing custom transports (e.g., LoRaWAN for long-range IoT).
- **Middleware Framework**: Customizable message processing with a chainable middleware system.
- **Type Safety**: Leverages CLOS with strict type declarations for messages and configurations.
- **AI-Driven Optimization**: Uses MGL neural networks for dynamic network topology optimization in Master mode.
- **Monitoring & Debugging**: Comprehensive metrics collection, Graphviz topology visualization, and an interactive TUI (ncurses-based).
- **StreamDB Integration**: Persistent storage for configurations, peer groups, metrics, and message logs using StreamDB, a lightweight, Rust-based key-value store with reverse trie indexing and thread-safe operations. Enhanced in v2.0.0 with full async bindings, transaction wrappers, JSON schema validation, WASM compatibility, granular error mapping, and retry logic.
- **Interoperability**: Supports gRPC/Protobuf for cross-language communication and StreamDB’s file-based storage for shared data access with other DCF SDKs.
- **Testing**: Integrated FiveAM test suite for robust validation, including StreamDB operations and extended benchmarks comparing StreamDB performance to in-memory storage.
- **Configuration**: JSON-based configuration with schema validation for reliability.
- **Production Enhancements (New in v2.0.0)**: Retry logic with exponential backoff for transient errors, thread-safe LRU caching for queries, SBCL-specific performance optimizations (e.g., type declarations), resource cleanup with `unwind-protect`, enhanced logging, and a deployment helper for building standalone executables.

## Getting Started

### Prerequisites

- **Common Lisp Environment**: SBCL (recommended) or another compatible Lisp implementation.
- **Quicklisp**: For dependency management ([install Quicklisp](https://www.quicklisp.org/)).
- **Dependencies**: Install via Quicklisp: `:cl-protobufs`, `:cl-grpc`, `:cffi`, `:uuid`, `:cl-json`, `:jsonschema`, `:cl-ppcre`, `:cl-csv`, `:usocket`, `:bordeaux-threads`, `:curses`, `:log4cl`, `:trivial-backtrace`, `:cl-store`, `:mgl`, `:hunchensocket`, `:fiveam`, `:cl-dot`, `:cl-lsquic`, `:cl-serial`, `:cl-can`, `:cl-sctp`, `:cl-zigbee`, `:cl-lorawan`.
- **StreamDB Library**: Ensure `libstreamdb.so` (or `.wasm` for WASM targets) is available and loadable via CFFI.

### Installation

1. Clone the repository:
   ```
   git clone https://github.com/ALH477/DeMoD-Communication-Framework --recurse-submodules
   cd DeMoD-Communication-Framework/lisp
   ```

2. Load the SDK in SBCL:
   ```
   sbcl --load src/d-lisp.lisp
   ```

3. (Optional) Build a standalone executable for deployment:
   ```lisp
   (dcf-deploy "dcf-lisp")
   ```

### Basic Usage

```lisp
;; Quick Start Client
(dcf-quick-start-client "config.json")

;; Send a Message
(dcf-quick-send "Hello, DCF!" "localhost:50052")

;; Persist Data to StreamDB
(dcf-db-insert "/test/key" "{\"data\": \"value\"}")

;; Query from StreamDB (with caching and validation)
(dcf-db-query "/test/key")

;; Async Query (non-blocking)
(dcf-db-get-async "/metrics/sends" (lambda (data len err) (if err (log:error err) (process-metrics data))))

;; Benchmark Performance
(run-benchmarks)
```

For WASM targets (e.g., browser-based nodes), ensure `libstreamdb.wasm` is loaded, and use async ops for UI-responsive queries.

## Advanced Usage

### StreamDB Integration (Enhanced in v2.0.0)

StreamDB serves as D-LISP's persistence layer, storing states, metrics, and logs with low-latency queries. Key enhancements:

- **Async Operations**: Non-blocking CRUD with callbacks (e.g., `dcf-db-get-async`) for real-time apps, integrated with D-LISP's event loop.
- **Transactions**: ACID batch ops via `dcf-db-begin-transaction-async`, `dcf-db-commit-transaction-async`, etc., for atomic peer updates.
- **Schema Validation**: JSON data validated against schemas (e.g., `*streamdb-metrics-schema*`) on insert/query for type safety.
- **WASM Compatibility**: No-mmap fallback for embedded/browser targets, with examples for UI config fetches.
- **Error Handling**: Granular mapping of StreamDB errors to `dcf-error` conditions, with backtraces and retries for transients.
- **Caching**: Thread-safe LRU cache for queries, reducing I/O in RTT-sensitive scenarios.
- **Benchmarks**: Compare StreamDB vs. in-memory performance, ensuring <1.5x overhead for 1000 operations.

Use Case: In IoT, persist sensor data atomically during batch transactions, querying async without blocking LoRaWAN receives.

### Plugins

Extend D-LISP with plugins for custom transports:

```lisp
(def-dcf-plugin my-transport
  ;; Plugin logic here
  )
(dcf-load-plugin "plugins/my-transport.lisp")
```

### Middleware

Customize message processing:

```lisp
(add-middleware (lambda (msg dir)
                  (log:debug "Processing ~A" msg)
                  msg))
```

### AI Optimization

In Master mode, optimize topology:

```lisp
(dcf-master-optimize-network)
```

Trains MGL nets on StreamDB-stored RTTs for efficient grouping.

### Testing

Run the FiveAM suite:

```lisp
(run-tests)
```

Includes StreamDB integration and type checks.

### CLI/TUI

Use CLI for commands (e.g., `db-insert [path] [data]`). Launch TUI for interactive monitoring:

```lisp
(dcf-tui)
```

### Deployment

Build executables:

```lisp
(dcf-deploy "my-app")
```

For WASM, compile with CL-WASM tools for browser deployment.

## Why D-LISP?

- **Performance**: Sub-ms messaging with StreamDB's low-latency persistence.
- **Modularity**: Plugins and middleware for easy customization.
- **Reliability**: Self-healing P2P, ACID transactions, and robust error handling.
- **Interoperability**: Cross-language via gRPC and StreamDB.
- **FOSS Ethos**: LGPL-3.0 license promotes reuse in libraries and apps.

## Acknowledgments

- **DeMoD LLC**: Core development and project leadership.
- **Grok 4 Heavy (xAI)**: AI-driven optimizations and code contributions.
- **Open-Source Community**: For libraries like `cl-grpc`, `cffi`, `mgl`, and `streamdb`.

## License

This project is licensed under the GNU Lesser General Public License v3.0 (LGPL-3.0). See `LICENSE` for details.

---

**DeMoD LLC** | Empowering Scalable, Fault-Tolerant Communication  
**Built with ❤️ and Lisp** | Powered by **Grok 4 Heavy** & **O B S E S S I O N**
