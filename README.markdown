# DeMoD Communications Framework (DCF)

**Version 5.0.0 | August 20, 2025**  
**Developed by DeMoD LLC**   
**License:** GNU General Public License v3.0 (GPL-3.0)  

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)  
https://demod.ltd/dcf.html


## Overview
The DeMoD Communications Framework (DCF) is a free and open-source software (FOSS) framework evolved from the DeMoD Secure Protocol, designed for low-latency, modular, and interoperable data exchange. It supports applications like IoT messaging, real-time gaming synchronization, distributed computing, and edge networking. DCF features a handshakeless design, efficient Protocol Buffers serialization, and a compatibility layer for UDP, TCP, WebSocket, and gRPC transports, enabling seamless peer-to-peer (P2P) networking with self-healing redundancy.

DCF is hardware and language agnostic, supporting embedded devices (e.g., Raspberry Pi), cloud servers, and mobile platforms (Android/iOS) with bindings in Perl, Python, C, C++, JavaScript (Node.js), Go, Rust, Java/Kotlin, and Swift. Version 5.0.0 introduces a plugin system for custom modules and transports, an AUTO mode for dynamic role assignment managed by a master node, and enhanced extensibility. Licensed under GPL-3.0, DCF ensures open-source derivatives. It includes CLI, TUI, server/client logic, P2P, and AUTO modes, making it versatile for standalone tools, libraries, or networked services. SDKs (e.g., C SDK) are developed as submodules for streamlined integration.

> **Important**: DCF complies with U.S. export regulations (EAR and ITAR). It avoids encryption to remain export-control-free. Users must ensure custom extensions comply; consult legal experts for specific use cases. DeMoD LLC disclaims liability for non-compliant modifications.

## Features
- **Modularity**: Independent components with standardized APIs; plugin system for custom extensions.
- **Interoperability**: Protocol Buffers and gRPC ensure cross-language (Perl, Python, C, C++, JS, Go, Rust, Java, Swift) and cross-platform compatibility.
- **Low Latency**: Sub-millisecond exchanges with <1% overhead; handshakeless design for real-time applications.
- **Flexibility**: Compatibility layer for UDP, TCP, WebSocket, gRPC, and custom transports via plugins; supports mobile bindings.
- **Dynamic Role Assignment**: AUTO mode allows nodes to switch between client, server, or P2P roles under master node control, enabling AI-driven network optimization.
- **Usability**: CLI for automation, TUI for monitoring; server, client, P2P, and AUTO modes with logging; master node commands for role and config management.
- **Self-Healing P2P**: Redundant paths, failure detection, RTT-based grouping (clusters by <50ms threshold), and optimized routing (Dijkstra with RTT weights).
- **Open Source**: GPL-3.0 ensures transparency and community contributions.

## Architecture
```mermaid
graph TD
    A[DCF Framework] --> B[CLI]
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
    G --> M[Self-Healing Redundancy]
    M --> N[Peer Discovery]
    M --> O[Failure Detection]
    M --> P[RTT-Based Grouping]
    
    D --> Q[Transport Layer]
    Q --> R[WebSocket]
    Q --> S[UDP]
    Q --> T[TCP]
    Q --> U[gRPC]
    Q --> V[Custom Plugins]
    
    D --> W[Protocol Buffers]
    W --> X[Serialization/Deserialization]
    
    A --> Y[Language Bindings]
    Y --> Z[Perl]
    Y --> AA[Python]
    Y --> AB[C/C++]
    Y --> AC[Node.js]
    Y --> AD[Go]
    Y --> AE[Rust]
    Y --> AF[Java/Kotlin]
    Y --> AG[Swift]
    
    A --> AH[Platform Support]
    AH --> AI[Embedded Devices]
    AH --> AJ[Cloud Servers]
    AH --> AK[Mobile: Android/iOS]
    
    A --> AL[SDKs]
    AL --> AM[C SDK]
    AL --> AN[Future SDKs: Python, Perl, etc.]
```

## Installation
Clone the repository with submodules:
```bash
git clone --recurse-submodules https://github.com/ALH477/DeMoD-Communication-Framework.git
cd DeMoD-Communication-Framework
```

### Prerequisites
- **Perl**: CPAN modules: `JSON`, `IO::Socket::INET`, `Getopt::Long`, `Curses::UI`, `Google::ProtocolBuffers::Dynamic`, `Grpc::XS`, `Module::Pluggable`.
- **Python**: `pip install protobuf grpcio grpcio-tools importlib`.
- **C SDK**: `libprotobuf-c`, `libuuid`, `libdl`, `libcjson`, `cmake`, `ncurses`.
- **C++**: `grpc`, `protobuf`.
- **Node.js**: `npm install @grpc/grpc-js @grpc/proto-loader`.
- **Go**: `go get google.golang.org/grpc github.com/golang/protobuf`.
- **Rust**: `cargo add tonic prost`.
- **Android**: Gradle: `io.grpc:grpc-okhttp`, `io.grpc:grpc-protobuf`, `io.grpc:grpc-stub`.
- **iOS**: Swift Package Manager: `grpc-swift`.

### Build Steps
1. **Generate Protobuf/gRPC Stubs**:
   ```bash
   # Perl/Python
   protoc --perl_out=perl/lib --python_out=python/dcf --grpc_out=python/dcf --plugin=protoc-gen-grpc_python=python -m grpc_tools.protoc messages.proto services.proto
   # C SDK
   protoc --c_out=c_sdk/src messages.proto
   # C++ SDK
   protoc --cpp_out=cpp/src --grpc_out=cpp/src --plugin=protoc-gen-grpc=grpc_cpp_plugin messages.proto services.proto
   # Node.js
   protoc --js_out=import_style=commonjs:nodejs/src --grpc_out=nodejs/src --plugin=protoc-gen-grpc=grpc_node_plugin messages.proto services.proto
   # Go
   protoc --go_out=go/src --go-grpc_out=go/src messages.proto services.proto
   # Rust
   # In rust/build.rs: tonic_build::compile_protos("proto/services.proto")
   # Android
   protoc --java_out=android/app/src/main --grpc_out=android/app/src/main --plugin=protoc-gen-grpc-java=grpc-java-plugin messages.proto services.proto
   # iOS
   protoc --swift_out=ios/Sources --grpc-swift_out=ios/Sources messages.proto services.proto
   ```
2. **Install Dependencies**:
   - C SDK: `sudo apt install libprotobuf-c-dev libuuid-dev libdl-dev libcjson-dev cmake libncurses-dev`
   - Perl: `cpanm --installdeps .`
   - Python: `pip install -r python/requirements.txt`
   - Others: Follow language-specific tools.
3. **Build SDKs**:
   - C SDK: `cd c_sdk && mkdir build && cd build && cmake .. && make`
   - Others: To be implemented (e.g., `pip install -e python/` for Python SDK).
4. **Plugins**: Place in `plugins/` or SDK-specific `plugins/` (e.g., `c_sdk/plugins/`).

## Usage Examples
### C SDK (P2P with RTT Grouping)
```c
// c_sdk/examples/p2p.c
#include <dcf_sdk/dcf_client.h>
#include <dcf_sdk/dcf_redundancy.h>
#include <stdio.h>
#include <uuid/uuid.h>

int main() {
    DCFClient* client = dcf_client_new();
    if (dcf_client_initialize(client, "config.json") != DCF_SUCCESS) {
        fprintf(stderr, "Init failed: %s\n", dcf_error_str(DCF_ERR_CONFIG_INVALID));
        return 1;
    }
    if (dcf_client_start(client) != DCF_SUCCESS) {
        fprintf(stderr, "Start failed\n");
        dcf_client_free(client);
        return 1;
    }
    char* response;
    const char* target = "localhost:50052";
    DCFError err = dcf_client_send_message(client, "P2P Hello!", target, &response);
    if (err == DCF_SUCCESS) {
        printf("Response: %s\n", response);
        free(response);
    } else {
        fprintf(stderr, "Send failed: %s\n", dcf_error_str(err));
    }
    int rtt;
    if (dcf_redundancy_health_check(client->redundancy, target, &rtt) == DCF_SUCCESS) {
        printf("RTT: %d ms\n", rtt);
    }
    dcf_client_stop(client);
    dcf_client_free(client);
    return 0;
}
```

### C SDK (AUTO Mode with Master Node)
```c
// c_sdk/examples/master.c
#include <dcf_sdk/dcf_master.h>
#include <cjson/cJSON.h>
#include <stdio.h>

int main() {
    DCFMaster* master = dcf_master_new();
    if (dcf_master_initialize(master, "master_config.json") != DCF_SUCCESS) {
        fprintf(stderr, "Master init failed\n");
        dcf_master_free(master);
        return 1;
    }
    if (dcf_master_start(master) != DCF_SUCCESS) {
        fprintf(stderr, "Master start failed\n");
        dcf_master_free(master);
        return 1;
    }
    // Assign role to a node
    if (dcf_master_assign_role(master, "peer1", P2P_MODE) == DCF_SUCCESS) {
        printf("Assigned P2P role to peer1\n");
    }
    // Update config
    if (dcf_master_update_config(master, "peer1", "rtt_threshold", "50") == DCF_SUCCESS) {
        printf("Updated RTT threshold for peer1\n");
    }
    // Collect metrics
    cJSON* metrics;
    if (dcf_master_collect_metrics(master, &metrics) == DCF_SUCCESS) {
        char* metrics_str = cJSON_PrintUnformatted(metrics);
        printf("Metrics: %s\n", metrics_str);
        free(metrics_str);
        cJSON_Delete(metrics);
    }
    dcf_master_stop(master);
    dcf_master_free(master);
    return 0;
}
```

### Python (gRPC Client)
```python
# python/dcf.py
from dcf.networking import Networking
net = Networking(transport="gRPC", host="localhost", port=50051)
print(net.send("Hello, DCF!", "recipient_id"))
```

### C++ (gRPC Server)
```cpp
// cpp/src/dcf.cpp
#include <grpcpp/grpcpp.h>
#include "services.grpc.pb.h"
class Networking : public DCFService::Service {
    grpc::Status SendMessage(grpc::ServerContext* context, const DCFMessage* request, DCFMessage* response) override {
        response->set_data("Echo: " + request->data());
        return grpc::Status::OK;
    }
};
int main() {
    grpc::ServerBuilder builder;
    Networking service;
    builder.AddListeningPort("0.0.0.0:50051", grpc::InsecureServerCredentials());
    builder.RegisterService(&service);
    std::unique_ptr<grpc::Server> server(builder.BuildAndStart());
    server->Wait();
    return 0;
}
```

### Node.js (gRPC Client)
```javascript
// nodejs/src/client.js
const grpc = require('@grpc/grpc-js');
const protoLoader = require('@grpc/proto-loader');
const packageDefinition = protoLoader.loadSync('services.proto');
const proto = grpc.loadPackageDefinition(packageDefinition).DCFService;
const client = new proto.DCFService('localhost:50051', grpc.credentials.createInsecure());
client.SendMessage({ data: 'Hello, DCF!' }, (err, response) => {
    if (!err) console.log(response.data);
});
```

### Go (gRPC Server)
```go
// go/src/main.go
package main
import (
    "context"
    "net"
    "google.golang.org/grpc"
    pb "path/to/generated/services"
)
type server struct { pb.UnimplementedDCFServiceServer }
func (s *server) SendMessage(ctx context.Context, in *pb.DCFMessage) (*pb.DCFMessage, error) {
    return &pb.DCFMessage{Data: "Echo: " + in.Data}, nil
}
func main() {
    lis, _ := net.Listen("tcp", ":50051");
    s := grpc.NewServer();
    pb.RegisterDCFServiceServer(s, &server{});
    s.Serve(lis);
}
```

### Rust (gRPC Server)
```rust
// rust/src/main.rs
use tonic::{transport::Server, Request, Response, Status};
use services::dcf_service_server::{DcfService, DcfServiceServer};
use services::{DcfMessage};
#[derive(Default)]
pub struct Networking {}
#[tonic::async_trait]
impl DcfService for Networking {
    async fn send_message(&self, request: Request<DcfMessage>) -> Result<Response<DcfMessage>, Status> {
        let reply = DcfMessage { data: format!("Echo: {}", request.into_inner().data) };
        Ok(Response::new(reply))
    }
}
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = "[::1]:50051".parse()?;
    let net = Networking::default();
    Server::builder().add_service(DcfServiceServer::new(net)).serve(addr).await?;
    Ok(())
}
```

### Android (Kotlin Client)
```kotlin
// android/app/src/main/kotlin/com/example/dcf/DCFClient.kt
import io.grpc.ManagedChannelBuilder
import com.example.dcf.services.DCFServiceGrpc
import com.example.dcf.messages.DCFMessage
class DCFClient(host: String, port: Int) {
    private val channel = ManagedChannelBuilder.forAddress(host, port).usePlaintext().build()
    private val stub = DCFServiceGrpc.newBlockingStub(channel)
    fun sendMessage(data: String, recipient: String): String {
        val request = DCFMessage.newBuilder().setData(data).setRecipient(recipient).build()
        return stub.sendMessage(request).data
    }
}
```

### iOS (Swift Client)
```swift
// ios/DCFClient.swift
import GRPC
import NIO
import SwiftProtobuf
class DCFClient {
    private let connection: ClientConnection
    private let client: DCFServiceClient
    init(host: String, port: Int) {
        let group = PlatformSupport.makeEventLoopGroup(loopCount: 1)
        connection = ClientConnection.insecure(group: group).connect(host: host, port: port)
        client = DCFServiceClient(channel: connection)
    }
    func sendMessage(data: String, recipient: String) -> String? {
        var request = DCFMessage()
        request.data = data
        request.recipient = recipient
        do {
            let response = try client.sendMessage(request).response.wait()
            return response.data
        } catch { return nil }
    }
}
```

### Plugin Example (C Transport for C SDK)
```c
// c_sdk/plugins/custom_transport.c
#include <dcf_sdk/dcf_plugin_manager.h>
typedef struct { /* Private data */ } CustomTransport;
bool setup(void* self, const char* host, int port) { return true; }
bool send(void* self, const uint8_t* data, size_t size, const char* target) { return true; }
uint8_t* receive(void* self, size_t* size) { *size = 0; return NULL; }
void destroy(void* self) { free(self); }
ITransport iface = {setup, send, receive, destroy};
void* create_plugin() { return calloc(1, sizeof(CustomTransport)); }
const char* get_plugin_version() { return "1.0"; }
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
    "transport": "libcustom_transport.so"
  }
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
- C SDK: `cd c_sdk/build && make test_redundancy test_plugin && valgrind --leak-check=full ./p2p`
- Perl: `prove -r tests/`
- Python: `pytest tests/`
- Others: To be implemented (e.g., `cargo test` for Rust).
- Integration: Use Docker for multi-language setups; test RTT grouping, failover, and AUTO mode role assignment.

## Contributing
Contributions are welcome! Follow these steps:
1. Fork the repo.
2. Create a feature branch (`git checkout -b feature/xyz`).
3. Add tests and code (follow style: `perltidy`, `black`, `ktlint`, `swiftformat`, `clang-format` for C).
4. Submit a PR with a clear description using the [PR template](docs/PR_TEMPLATE.md).
5. Discuss issues via [GitHub Issues](https://github.com/demodllc/dcf/issues).
New SDKs (e.g., Python, Perl) encouraged; ensure RTT grouping, plugins, AUTO mode, and GPL-3.0 compliance.

## Documentation
See `docs/dcf_design_spec.md` for detailed architecture, SDK guidelines, plugin system, AUTO mode, and testing. C SDK documentation in `c_sdk/C-SDKreadme.markdown`.
