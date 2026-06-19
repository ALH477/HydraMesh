# DCF-Mesh — self-healing redundancy over the wire

Status: the **certified algorithm layer** (peer-liveness FSM, RTT grouping,
RTT-weighted Dijkstra, route failover, master election) and the **REPORT/ROLE
control adapter** are byte/value-identical across Python (the reference,
`python/MCP/meshlab_core.py`), C, Rust, and Go, each diffed against the shared golden
vectors (`Documentation/mesh_vectors.json`). The **self-healing runtime** that drives
those algorithms from live network state is implemented and verified live in the
**Go, C, Rust, and Python** nodes.

This is not a new wire format. DCF-Mesh is an **adapter over `DeModFrame`**: every
control message is the payload of an ordinary `ProtoMessage` of type `MsgMesh = 11`
(see `WIRE_QUANTUM_SPEC.md` for the quantum and the envelope). Self-healing is opt-in
— a node runs it only in `auto`/`master` mode; plain `p2p` nodes are unaffected.

## Motivation

A DCF mesh must keep working as nodes come and go, without a single point of failure
and without any cryptographic handshake (the wire is deliberately encryption-free —
see `DCF_SECURITY_EXPOSURE.md`). DCF-Mesh gives each node a live, deterministic view
of its peers' health and the mesh topology, lets an elected master assign relay/leaf
roles for efficient forwarding, and — crucially — re-elects a new master locally and
deterministically the instant the old one becomes unreachable. The algorithms are
pure integer functions over small-int node ids, so every language agrees bit-for-bit;
the runtime is the thin, per-language shell that feeds them PING/PONG and REPORT/ROLE.

## 1. The certified algorithm layer

Pure, deterministic functions over node ids `0..n-1` (no I/O, no floats on the
contract path). Status constants `HEALTHY=0 / DEGRADED=1 / UNREACHABLE=2`; role
constants `LEAF=0 / RELAY=1 / MASTER=2`; `INF=0x3FFFFFFF`, `NO_HOP=-1`,
`ISOLATED=-1`.

| Primitive | Contract |
|-----------|----------|
| `peer_status(events, fail_thr, ok_thr)` | Fold a health-check sequence (`1`=ok, `0`=timeout) into a status. **Sticky** `UNREACHABLE`: once `fail_thr` consecutive timeouts hit, the peer stays `UNREACHABLE` until `ok_thr` consecutive successes recover it; a single timeout from `HEALTHY` is `DEGRADED`. |
| `group_of(rtt_ms, threshold_ms, status)` | `floor(rtt/threshold)`, or `ISOLATED` if the peer is `UNREACHABLE`. |
| `dijkstra(n, edges, source)` | RTT-weighted shortest paths → `(dist, next_hop)`. Deterministic tie-break: smaller predecessor id. |
| `select_routes(candidates)` | Order `(route_id, status, rtt)` candidates into a failover list: drop `UNREACHABLE`, then sort by `(status, rtt, route_id)` ascending. |
| `elect(n, edges, relay_min_degree)` | Master = max healthy degree, tie-broken by smallest average RTT (floored), then smallest id. Nodes with degree `≥ relay_min_degree` are `RELAY`, the rest `LEAF`; the master is `MASTER`. |

## 2. The control adapter (REPORT / ROLE)

Big-endian, byte-exact, carried as the `MsgMesh` payload. Header byte 0 is the
control type, byte 1 is `MESH_VERSION = 1`.

**REPORT** (`type = 0`, node → master) — a node's view of its peers, `5 + 5·n` bytes:

```
type(1)=0 | ver(1)=1 | node_id(2) | n_peers(1) | n_peers × [ peer_id(2) | status(1) | rtt_ms(2) ]
```

**ROLE** (`type = 1`, master → node) — an assigned role, 7 bytes:

```
type(1)=1 | ver(1)=1 | node_id(2) | role(1) | master_id(2)
```

`pack_report`/`unpack_report` and `pack_role`/`unpack_role` live next to each
language's mesh module (table below). Adding fields is a version bump, never a silent
layout change — the golden vectors are the contract.

## 3. The self-healing runtime

A node in `auto`/`master` mode runs one tick (~**1 s**) that: (a) folds each peer's
PONG-or-timeout into a 5-event sliding window and recomputes its status via
`peer_status`, (b) PINGs every peer, then (c) does mode-specific work. Tuning is
identical in every language: window cap **5**, fail threshold **3**, ok threshold
**2**, relay min degree **2**; the `mesh-status` line is logged every **3rd** tick.

- **`p2p`** — health FSM only (no control traffic, no roles).
- **`auto`** — REPORT its peer view (status + RTT) to the configured master peer, and
  run **decentralized failover**: if that master peer is `UNREACHABLE`, locally
  re-elect the lowest-id healthy node (becoming master itself if it is that node). No
  central coordinator is consulted, so the mesh heals even if the master vanishes.
- **`master`** — aggregate every reporter's edges into a deduplicated, canonicalized
  topology (dropping `UNREACHABLE` links, keeping the min-RTT weight per edge), map
  ids to a dense `0..N-1` space, run `elect`, adopt its own role, and unicast each
  reporter its `ROLE`.

Status line (one per 3 ticks):

```
mesh-status node=<id> mode=<p2p|auto|master> role=<leaf|relay|master> master=<id> peers=[<id>:<status>:g<group> ...]
```

## 4. Reference implementations

| Lang | Certified algorithms + control | Self-healing runtime | Node / CLI |
|------|--------------------------------|----------------------|------------|
| C | `codec/demod_mesh.h` | `C_SDK/node/dcf_mesh_runtime.h` | `dcfnode start` |
| Rust | `codec/src/mesh.rs` | `rust/src/mesh_runtime.rs` | `dcf mesh` |
| Go | `go/mesh/mesh.go` | `go/node/mesh_runtime.go` | `dcfnode start` |
| Python | `python/MCP/meshlab_core.py` | `python/dcf/mesh_runtime.py` (+ `dcf/udp_node.py`, `dcf/proto.py`) | `python/dcf_node.py start` |

All four CLIs share the same flags:

```
<node> start --mode p2p|auto|master --node-id N [--master PEER] --peer id@host:port ...
```

(`--bind host:port` selects the local UDP address; `--peer` is repeatable. The Rust
binary spells the subcommand `dcf mesh ...`.) Because the envelope and the control
bytes are identical, the nodes interoperate: a Go master will assign roles to a Rust
or Python `auto` node, and killing it triggers the same local re-election everywhere.

## 5. Verification

The certificate is the contract — regenerate and diff on any change to a mesh module:

```sh
python3 python/MCP/gen_mesh_vectors.py /tmp/mv.json                              # Python: regen + verify laws
cd codec && cargo test --test certify_mesh                                       # Rust
gcc -std=c11 -I codec C_SDK/tests/test_mesh_certify.c -lm -o /tmp/mc && /tmp/mc  # C
cd go && go test ./mesh/                                                         # Go
```

Vectors: `Documentation/mesh_vectors.json` (+ identical `python/MCP/` copy) and the
dependency-free C header `codec/mesh_vectors.gen.h`. The runtime's *timing* is not
golden-vectored (it is integration-tested); the algorithms and control bytes are.

Live mixed-language mesh (the end-to-end proof — start three nodes, then kill the
master and watch the survivors re-elect):

```sh
# master (Go)
go run ./go/cmd/dcfnode start --mode master --node-id 0 --bind 0.0.0.0:7000 \
  --peer 1@localhost:7001 --peer 2@localhost:7002
# auto (Rust)
cd rust && cargo run -- mesh --mode auto --node-id 1 --bind 0.0.0.0:7001 \
  --master 0 --peer 0@localhost:7000 --peer 2@localhost:7002
# auto (Python)
python3 python/dcf_node.py start --mode auto --node-id 2 --bind 0.0.0.0:7002 \
  --master 0 --peer 0@localhost:7000 --peer 1@localhost:7001
```

## 6. Security

DCF-Mesh control is **plaintext**, like the rest of the wire. An on-path observer can
read the full topology, RTTs, and role assignments from `REPORT`/`ROLE`, and an active
attacker can forge them (there is no authentication). This is by design for export
compliance; deploy the mesh inside WireGuard or operator-supplied crypto **beneath**
the UDP socket. See `DCF_SECURITY_EXPOSURE.md` for the threat model, a runnable
wiretap demo, and the deployment rule.
