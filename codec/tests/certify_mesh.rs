// SPDX-License-Identifier: LGPL-3.0-only
//! Rust certification for the DCF self-healing mesh algorithms — diffs the Rust
//! implementation against the cross-language golden vectors
//! (Documentation/mesh_vectors.json). Passing == value-agreement with the Python
//! and C references on all five primitives.

use dcf_wire_codec::mesh;
use serde::Deserialize;
use std::path::Path;

#[derive(Deserialize)]
struct Vectors {
    fsm: Vec<Fsm>,
    grouping: Vec<Group>,
    dijkstra: Vec<Dij>,
    routes: Vec<Route>,
    election: Vec<Elect>,
    control: Control,
}
#[derive(Deserialize)]
struct Control { report: Vec<Report>, role: Vec<Role> }
#[derive(Deserialize)]
struct Report { node_id: i32, peers: Vec<[i32; 3]>, bytes: String }
#[derive(Deserialize)]
struct Role { node_id: i32, role: i32, master_id: i32, bytes: String }
#[derive(Deserialize)]
struct Fsm { events: Vec<u8>, fail_threshold: i32, ok_threshold: i32, status: i32 }
#[derive(Deserialize)]
struct Group { rtt: i32, threshold: i32, status: i32, group: i32 }
#[derive(Deserialize)]
struct Dij { n: usize, edges: Vec<[i32; 3]>, source: usize, dist: Vec<i32>, next_hop: Vec<i32> }
#[derive(Deserialize)]
struct Route { candidates: Vec<[i32; 3]>, ordered: Vec<i32> }
#[derive(Deserialize)]
struct Elect { n: usize, edges: Vec<[i32; 3]>, relay_min_degree: i32, master: i32, roles: Vec<i32> }

fn load() -> Vectors {
    let dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    for p in [
        format!("{}/../Documentation/mesh_vectors.json", dir),
        format!("{}/../python/MCP/mesh_vectors.json", dir),
    ] {
        if Path::new(&p).exists() {
            let data = std::fs::read_to_string(&p).unwrap();
            return serde_json::from_str(&data).unwrap_or_else(|e| panic!("parse {}: {}", p, e));
        }
    }
    panic!("mesh_vectors.json not found (run gen_mesh_vectors.py)");
}

#[test]
fn all_primitives_match_golden() {
    let v = load();
    for c in &v.fsm {
        assert_eq!(mesh::peer_status(&c.events, c.fail_threshold, c.ok_threshold), c.status, "fsm");
    }
    for c in &v.grouping {
        assert_eq!(mesh::group_of(c.rtt, c.threshold, c.status), c.group, "grouping");
    }
    for c in &v.dijkstra {
        let (dist, nh) = mesh::dijkstra(c.n, &c.edges, c.source);
        assert_eq!(dist, c.dist, "dijkstra dist");
        assert_eq!(nh, c.next_hop, "dijkstra next_hop");
    }
    for c in &v.routes {
        assert_eq!(mesh::select_routes(&c.candidates), c.ordered, "routes");
    }
    for c in &v.election {
        let (m, roles) = mesh::elect(c.n, &c.edges, c.relay_min_degree);
        assert_eq!(m, c.master, "election master");
        assert_eq!(roles, c.roles, "election roles");
    }
    let hex = |b: &[u8]| b.iter().map(|x| format!("{:02x}", x)).collect::<String>();
    for c in &v.control.report {
        let packed = mesh::pack_report(c.node_id, &c.peers);
        assert_eq!(hex(&packed), c.bytes, "report pack");
        let raw: Vec<u8> = (0..c.bytes.len()).step_by(2)
            .map(|i| u8::from_str_radix(&c.bytes[i..i + 2], 16).unwrap()).collect();
        let (nid, peers) = mesh::unpack_report(&raw).expect("unpack report");
        assert_eq!((nid, peers), (c.node_id, c.peers.clone()), "report unpack");
    }
    for c in &v.control.role {
        let packed = mesh::pack_role(c.node_id, c.role, c.master_id);
        assert_eq!(hex(&packed), c.bytes, "role pack");
        let raw: Vec<u8> = (0..c.bytes.len()).step_by(2)
            .map(|i| u8::from_str_radix(&c.bytes[i..i + 2], 16).unwrap()).collect();
        assert_eq!(mesh::unpack_role(&raw), Some((c.node_id, c.role, c.master_id)), "role unpack");
    }
    println!(
        "PASS  mesh: {} fsm, {} grouping, {} dijkstra, {} routes, {} election",
        v.fsm.len(), v.grouping.len(), v.dijkstra.len(), v.routes.len(), v.election.len()
    );
}
