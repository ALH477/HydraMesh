// SPDX-License-Identifier: LGPL-3.0-only
//
// C++ supercharged gRPC DCF node — server, client, and CLI. See dcf/dcf_node.hpp.

#include "dcf/dcf_node.hpp"

#include <chrono>
#include <cstdint>
#include <cstdio>
#include <memory>
#include <string>
#include <vector>

#include <grpcpp/grpcpp.h>
#include <grpcpp/health_check_service_interface.h>
#include <grpcpp/ext/proto_server_reflection_plugin.h>

#include "dcf.grpc.pb.h"
#include "dcf/frame.hpp"
#include "dcf/superpack.hpp"

using grpc::Channel;
using grpc::ClientContext;
using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::ServerReaderWriter;
using grpc::ServerWriter;
using grpc::Status;

static uint64_t now_us() {
    using namespace std::chrono;
    return (uint64_t)duration_cast<microseconds>(system_clock::now().time_since_epoch()).count();
}

static std::string from_hex(const std::string& h) {
    std::string out;
    for (size_t i = 0; i + 1 < h.size(); i += 2)
        out.push_back((char)std::stoul(h.substr(i, 2), nullptr, 16));
    return out;
}

static std::string golden_frame() {
    dcf::Frame f;
    f.version = 1; f.type = 3; f.seq = 0x1234; f.src = 1; f.dst = dcf::BROADCAST;
    f.payload = {0xDE, 0xAD, 0xBE, 0xEF}; f.ts_us = 0xAB12CD;
    auto b = f.encode();
    return std::string(reinterpret_cast<const char*>(b.data()), b.size());
}

// Validate + describe an envelope's certified payload (frame / superpack).
static std::string describe(const dcf::Envelope& e) {
    const std::string& p = e.payload();
    try {
        if (e.kind() == dcf::KIND_SUPERPACK && p.size() == dcf::SUPER_LEN) {
            auto [a, b] = dcf::unpack(reinterpret_cast<const uint8_t*>(p.data()), p.size());
            (void)a; (void)b;
            return "superpack (2 frames, valid)";
        }
        if (p.size() == dcf::FRAME_SIZE) {
            dcf::decode(reinterpret_cast<const uint8_t*>(p.data()), p.size());
            return "frame (valid)";
        }
    } catch (const std::exception& ex) {
        return std::string("INVALID: ") + ex.what();
    }
    return "payload (" + std::to_string(p.size()) + " bytes)";
}

// ── Server ──────────────────────────────────────────────────────────────────
class DCFServiceImpl final : public dcf::DCFService::Service {
public:
    explicit DCFServiceImpl(std::string node_id) : node_id_(std::move(node_id)) {}

    Status SendFrame(ServerContext*, const dcf::Envelope* req, dcf::Ack* ack) override {
        std::printf("[server] SendFrame from %s: %s\n", req->node_id().c_str(), describe(*req).c_str());
        std::fflush(stdout);
        ack->set_ok(true);
        ack->set_detail(node_id_);
        return Status::OK;
    }

    Status Ping(ServerContext*, const dcf::PingRequest* req, dcf::PongReply* reply) override {
        reply->set_node_id(node_id_);
        reply->set_timestamp(req->timestamp());  // echo for RTT
        return Status::OK;
    }

    Status Subscribe(ServerContext*, const dcf::Subscription* sub,
                     ServerWriter<dcf::Envelope>* writer) override {
        // Minimal: emit a single heartbeat frame so subscribers see the certified wire.
        dcf::Envelope e;
        e.set_kind(dcf::KIND_FRAME);
        e.set_node_id(node_id_);
        e.set_timestamp(now_us());
        e.set_payload(golden_frame());
        writer->Write(e);
        std::printf("[server] Subscribe from %s (channel %u)\n", sub->node_id().c_str(), sub->channel());
        std::fflush(stdout);
        return Status::OK;
    }

    // The supercharged path: bidirectional streaming mesh.
    Status MeshStream(ServerContext*, ServerReaderWriter<dcf::Envelope, dcf::Envelope>* stream) override {
        dcf::Envelope in;
        while (stream->Read(&in)) {
            dcf::Envelope out;
            out.set_node_id(node_id_);
            out.set_timestamp(now_us());
            if (in.kind() == dcf::KIND_PING) {
                out.set_kind(dcf::KIND_PONG);
                out.set_timestamp(in.timestamp());
            } else {
                std::printf("[server] mesh recv from %s: %s\n", in.node_id().c_str(), describe(in).c_str());
                std::fflush(stdout);
                out.set_kind(in.kind());
                out.set_payload(in.payload());  // echo the certified bytes back
            }
            if (!stream->Write(out)) break;
        }
        return Status::OK;
    }

private:
    std::string node_id_;
};

namespace dcf {

int run_server(const std::string& listen_addr, const std::string& node_id) {
    grpc::EnableDefaultHealthCheckService(true);
    grpc::reflection::InitProtoReflectionServerBuilderPlugin();
    DCFServiceImpl service(node_id);
    ServerBuilder builder;
    builder.AddListeningPort(listen_addr, grpc::InsecureServerCredentials());
    builder.RegisterService(&service);
    std::unique_ptr<Server> server(builder.BuildAndStart());
    if (!server) { std::fprintf(stderr, "failed to bind %s\n", listen_addr.c_str()); return 1; }
    std::printf("dcfcpp serving on %s as %s (health + reflection enabled)\n", listen_addr.c_str(), node_id.c_str());
    std::fflush(stdout);
    server->Wait();
    return 0;
}

static std::unique_ptr<DCFService::Stub> stub_for(const std::string& peer) {
    return DCFService::NewStub(grpc::CreateChannel(peer, grpc::InsecureChannelCredentials()));
}

int run_client(const std::string& peer, const std::string& node_id) {
    auto stub = stub_for(peer);
    ClientContext ctx;
    auto stream = stub->MeshStream(&ctx);

    // 1) PING for RTT
    Envelope ping;
    ping.set_kind(KIND_PING);
    ping.set_node_id(node_id);
    ping.set_timestamp(now_us());
    stream->Write(ping);

    // 2) a certified frame, 3) a SuperPack (two frames)
    std::string frame = golden_frame();
    Envelope fe; fe.set_kind(KIND_FRAME); fe.set_node_id(node_id); fe.set_timestamp(now_us()); fe.set_payload(frame);
    stream->Write(fe);

    Frame fa; fa.version = 1; fa.type = 0; fa.seq = 1; fa.src = 1; fa.dst = 2; fa.payload = {1, 2, 3, 4}; fa.ts_us = 1;
    auto a = fa.encode(); auto b = Frame{}.encode();
    auto sp = pack(a, b);
    Envelope se; se.set_kind(KIND_SUPERPACK); se.set_node_id(node_id); se.set_timestamp(now_us());
    se.set_payload(std::string(reinterpret_cast<const char*>(sp.data()), sp.size()));
    stream->Write(se);

    stream->WritesDone();

    Envelope resp;
    int got = 0;
    while (stream->Read(&resp)) {
        got++;
        if (resp.kind() == KIND_PONG) {
            uint64_t rtt = now_us() - resp.timestamp();
            std::printf("[client] PONG from %s, RTT=%llu us\n", resp.node_id().c_str(), (unsigned long long)rtt);
        } else {
            std::printf("[client] mesh echo from %s: %s\n", resp.node_id().c_str(), describe(resp).c_str());
        }
    }
    Status s = stream->Finish();
    std::fflush(stdout);
    if (!s.ok()) { std::fprintf(stderr, "MeshStream failed: %s\n", s.error_message().c_str()); return 1; }
    return got > 0 ? 0 : 1;
}

int send_frame(const std::string& peer, const std::string& hex) {
    auto stub = stub_for(peer);
    Envelope req;
    req.set_kind(KIND_FRAME);
    req.set_node_id("dcfcpp");
    req.set_timestamp(now_us());
    req.set_payload(hex.empty() ? golden_frame() : from_hex(hex));
    ClientContext ctx;
    Ack ack;
    Status s = stub->SendFrame(&ctx, req, &ack);
    if (!s.ok()) { std::fprintf(stderr, "SendFrame failed: %s\n", s.error_message().c_str()); return 1; }
    std::printf("ack ok=%d from %s\n", ack.ok(), ack.detail().c_str());
    return ack.ok() ? 0 : 1;
}

int bench(const std::string& peer, int count) {
    auto stub = stub_for(peer);
    uint64_t total = 0;
    int ok = 0;
    for (int i = 0; i < count; i++) {
        PingRequest req; req.set_node_id("dcfcpp"); req.set_timestamp(now_us());
        ClientContext ctx; PongReply reply;
        uint64_t t0 = now_us();
        if (stub->Ping(&ctx, req, &reply).ok()) { total += now_us() - t0; ok++; }
    }
    if (ok == 0) { std::fprintf(stderr, "all pings failed\n"); return 1; }
    std::printf("bench: %d/%d pings, mean RTT=%llu us\n", ok, count, (unsigned long long)(total / ok));
    return 0;
}

}  // namespace dcf

// ── CLI ─────────────────────────────────────────────────────────────────────
static const char* opt(int argc, char** argv, const char* key, const char* def) {
    for (int i = 0; i < argc; i++)
        if (std::string(argv[i]) == key && i + 1 < argc) return argv[i + 1];
    return def;
}

static void print_usage() {
    std::printf(
        "dcfcpp 0.3.0 — DCF C++ gRPC node (DeModFrame + SuperPack over gRPC bidi MeshStream)\n"
        "  serve      [--port 50051] [--node-id ID]\n"
        "  connect    --peer host:port [--node-id ID]\n"
        "  send-frame --peer host:port [--hex BYTES]\n"
        "  bench      --peer host:port [--count 100]\n");
}

int main(int argc, char** argv) {
    if (argc < 2) { print_usage(); return 2; }
    std::string cmd = argv[1];
    if (cmd == "version" || cmd == "--version" || cmd == "-v" || cmd == "help" || cmd == "--help") {
        print_usage();
        return 0;
    }
    int rc = argc - 2; char** ra = argv + 2;
    if (cmd == "serve") {
        std::string port = opt(rc, ra, "--port", "50051");
        return dcf::run_server("0.0.0.0:" + port, opt(rc, ra, "--node-id", "dcfcpp-node"));
    }
    if (cmd == "connect") {
        const char* peer = opt(rc, ra, "--peer", nullptr);
        if (!peer) { std::fprintf(stderr, "--peer required\n"); return 2; }
        return dcf::run_client(peer, opt(rc, ra, "--node-id", "dcfcpp-client"));
    }
    if (cmd == "send-frame") {
        const char* peer = opt(rc, ra, "--peer", nullptr);
        if (!peer) { std::fprintf(stderr, "--peer required\n"); return 2; }
        return dcf::send_frame(peer, opt(rc, ra, "--hex", ""));
    }
    if (cmd == "bench") {
        const char* peer = opt(rc, ra, "--peer", nullptr);
        if (!peer) { std::fprintf(stderr, "--peer required\n"); return 2; }
        return dcf::bench(peer, std::atoi(opt(rc, ra, "--count", "100")));
    }
    std::fprintf(stderr, "unknown command %s\n", cmd.c_str());
    return 2;
}
