// SPDX-License-Identifier: LGPL-3.0-only
//
// dcfcpp — the C++ DCF node. Two transports, selected at build time:
//   • gRPC  (DCF_CPP_GRPC): unary SendFrame, bidi MeshStream, Ping/RTT (legacy).
//   • Steam (DCF_CPP_GNS / DCF_CPP_STEAM): ISteamNetworkingSockets — a dedicated
//     server "hub" + clients, the Steam-compatible multiplayer transport.
// See dcf/dcf_node.hpp and Documentation/DCF_STEAM_SPEC.md.

#include "dcf/dcf_node.hpp"

#include <atomic>
#include <chrono>
#include <csignal>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <string>
#include <thread>
#include <vector>

#include "dcf/frame.hpp"
#include "dcf/superpack.hpp"

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

static std::string to_hex(const std::string& b) {
    static const char* k = "0123456789abcdef";
    std::string out;
    out.reserve(b.size() * 2);
    for (unsigned char c : b) {
        out.push_back(k[c >> 4]);
        out.push_back(k[c & 0xF]);
    }
    return out;
}

static std::string golden_frame() {
    dcf::Frame f;
    f.version = 1; f.type = 3; f.seq = 0x1234; f.src = 1; f.dst = dcf::BROADCAST;
    f.payload = {0xDE, 0xAD, 0xBE, 0xEF}; f.ts_us = 0xAB12CD;
    auto b = f.encode();
    return std::string(reinterpret_cast<const char*>(b.data()), b.size());
}

// ════════════════════════════ gRPC transport (legacy) ════════════════════════
#if defined(DCF_CPP_GRPC)
#include <grpcpp/grpcpp.h>
#include <grpcpp/health_check_service_interface.h>
#include <grpcpp/ext/proto_server_reflection_plugin.h>
#include "dcf.grpc.pb.h"

using grpc::Channel;
using grpc::ClientContext;
using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::ServerReaderWriter;
using grpc::ServerWriter;
using grpc::Status;

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
                out.set_payload(in.payload());
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

    Envelope ping;
    ping.set_kind(KIND_PING);
    ping.set_node_id(node_id);
    ping.set_timestamp(now_us());
    stream->Write(ping);

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
#endif  // DCF_CPP_GRPC

// ═══════════════════════ Steam-compatible transport (GNS / Steamworks) ════════
#if defined(DCF_CPP_GNS) || defined(DCF_CPP_STEAM)
#include "dcf/net_steam.hpp"
#if defined(DCF_CPP_STEAM)
#include "dcf/steam_ext.hpp"
#endif

static std::atomic<bool> g_running{true};
static void on_sigint(int) { g_running = false; }

namespace dcf {

// Dedicated server / hub: accept clients and forward each DCF frame to the others,
// preserving the sender's reliability. Channel filtering is the receiver's job
// (the DCF dst-channel rendezvous), exactly as elsewhere in DCF.
int run_gns_server(uint16_t port, const std::string& node_id, bool steam) {
    SteamNet net(node_id);
    if (!net.ok()) return 1;
#if defined(DCF_CPP_STEAM)
    if (steam) steam_server_register(node_id.c_str(), port);
#endif
    if (!net.listen(port)) { std::fprintf(stderr, "[hub] listen on %u failed\n", port); return 1; }
    std::signal(SIGINT, on_sigint);
    std::signal(SIGTERM, on_sigint);
    std::printf("dcfcpp %s hub on port %u as %s — forwarding DCF frames by channel\n",
                steam ? "serve-steam (SDR)" : "serve-gns", port, node_id.c_str());
    std::fflush(stdout);

    std::vector<NetMessage> in;
    uint64_t frames = 0;
    while (g_running) {
        in.clear();
        net.poll(in);
#if defined(DCF_CPP_STEAM)
        if (steam) steam_server_run_callbacks();  // master-server heartbeats
#endif
        for (auto& m : in) {
            const int fwd = net.broadcast_except(m.from, m.data.data(), m.data.size(), m.reliable);
            uint16_t dst = 0;
            const char* what = "payload";
            if (m.data.size() == dcf::FRAME_SIZE) {
                try {
                    auto f = dcf::decode(reinterpret_cast<const uint8_t*>(m.data.data()), m.data.size());
                    dst = f.dst; what = "frame";
                } catch (...) { what = "invalid"; }
            } else if (m.data.size() == dcf::SUPER_LEN) {
                what = "superpack";
            }
            std::printf("[hub] %s %zuB dst=0x%04X %s from #%u -> %d peer(s)\n",
                        what, m.data.size(), dst, m.reliable ? "rel" : "unrel",
                        (unsigned)m.from, fwd);
            std::fflush(stdout);
            ++frames;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(2));
    }
#if defined(DCF_CPP_STEAM)
    if (steam) steam_server_shutdown();
#endif
    std::printf("[hub] stopped after %llu frame(s)\n", (unsigned long long)frames);
    return 0;
}

// Client: connect to a hub (GNS host:port, or Steam peer identity), optionally send
// one DCF frame, and print frames received for `listen_secs`.
int run_gns_client(const std::string& peer, const std::string& node_id,
                   const std::string& send_hex, bool reliable, int listen_secs, bool steam) {
    std::string host = peer;
    uint16_t port = 0;
    if (!steam) {
        auto pos = peer.rfind(':');
        if (pos == std::string::npos) { std::fprintf(stderr, "[client] --peer must be host:port\n"); return 2; }
        host = peer.substr(0, pos);
        port = (uint16_t)std::atoi(peer.substr(pos + 1).c_str());
    }
    SteamNet net(node_id);
    if (!net.ok()) return 1;
    ConnId c = net.connect(host, port);
    if (!c) { std::fprintf(stderr, "[client] connect to %s failed\n", peer.c_str()); return 1; }

    const std::string frame = send_hex.empty() ? std::string() : from_hex(send_hex);
    bool sent = send_hex.empty();
    std::vector<NetMessage> in;
    const uint64_t t0 = now_us();
    int received = 0;
    while ((now_us() - t0) < (uint64_t)listen_secs * 1000000ULL) {
        in.clear();
        net.poll(in);
        for (auto& m : in) {
            ++received;
            std::printf("[client %s] recv %zuB %s: %s\n", node_id.c_str(), m.data.size(),
                        m.reliable ? "rel" : "unrel", to_hex(m.data).c_str());
            std::fflush(stdout);
        }
        if (!sent && net.client_connected()) {
            if (net.send(c, frame.data(), frame.size(), reliable)) {
                std::printf("[client %s] sent %zuB %s: %s\n", node_id.c_str(), frame.size(),
                            reliable ? "rel" : "unrel", send_hex.c_str());
                std::fflush(stdout);
                sent = true;
            }
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(5));
    }
    std::printf("[client %s] done: %d frame(s) received\n", node_id.c_str(), received);
    return sent ? 0 : 1;
}

}  // namespace dcf
#endif  // DCF_CPP_GNS || DCF_CPP_STEAM

// ════════════════════════════════════ CLI ════════════════════════════════════
static const char* opt(int argc, char** argv, const char* key, const char* def) {
    for (int i = 0; i < argc; i++)
        if (std::string(argv[i]) == key && i + 1 < argc) return argv[i + 1];
    return def;
}

static bool has_flag(int argc, char** argv, const char* key) {
    for (int i = 0; i < argc; i++)
        if (std::string(argv[i]) == key) return true;
    return false;
}

static void print_usage() {
    std::printf("dcfcpp 0.3.0 — DCF C++ node (DeModFrame + SuperPack)\n");
#if defined(DCF_CPP_GRPC)
    std::printf(
        "  gRPC transport:\n"
        "    serve      [--port 50051] [--node-id ID]\n"
        "    connect    --peer host:port [--node-id ID]\n"
        "    send-frame --peer host:port [--hex BYTES]\n"
        "    bench      --peer host:port [--count 100]\n");
#endif
#if defined(DCF_CPP_GNS) || defined(DCF_CPP_STEAM)
    std::printf(
        "  Steam-compatible transport (GameNetworkingSockets):\n"
        "    serve-gns     [--port 27015] [--node-id ID]            (dedicated server / hub)\n"
        "    connect-gns   --peer host:port [--send-hex BYTES] [--reliable] [--listen SECS]\n");
#endif
#if defined(DCF_CPP_STEAM)
    std::printf(
        "  Steamworks (SDR relay + lobbies; needs the Steamworks SDK build):\n"
        "    serve-steam   [--port 16000] [--node-id ID]\n"
        "    connect-steam --peer IDENTITY [--send-hex BYTES] [--reliable] [--listen SECS]\n");
#endif
}

int main(int argc, char** argv) {
    if (argc < 2) { print_usage(); return 2; }
    std::string cmd = argv[1];
    if (cmd == "version" || cmd == "--version" || cmd == "-v" || cmd == "help" || cmd == "--help") {
        print_usage();
        return 0;
    }
    int rc = argc - 2; char** ra = argv + 2;

#if defined(DCF_CPP_GRPC)
    if (cmd == "serve")
        return dcf::run_server("0.0.0.0:" + std::string(opt(rc, ra, "--port", "50051")),
                               opt(rc, ra, "--node-id", "dcfcpp-node"));
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
#endif

#if defined(DCF_CPP_GNS) || defined(DCF_CPP_STEAM)
    if (cmd == "serve-gns" || cmd == "serve-steam") {
        const bool steam = (cmd == "serve-steam");
        return dcf::run_gns_server((uint16_t)std::atoi(opt(rc, ra, "--port", steam ? "16000" : "27015")),
                                   opt(rc, ra, "--node-id", "dcfcpp-hub"), steam);
    }
    if (cmd == "connect-gns" || cmd == "connect-steam") {
        const bool steam = (cmd == "connect-steam");
        const char* peer = opt(rc, ra, "--peer", steam ? nullptr : "127.0.0.1:27015");
        if (!peer) { std::fprintf(stderr, "--peer required\n"); return 2; }
        return dcf::run_gns_client(peer, opt(rc, ra, "--node-id", "dcfcpp-client"),
                                   opt(rc, ra, "--send-hex", ""), has_flag(rc, ra, "--reliable"),
                                   std::atoi(opt(rc, ra, "--listen", "2")), steam);
    }
#endif

    std::fprintf(stderr, "unknown or unavailable command: %s\n", cmd.c_str());
    return 2;
}
