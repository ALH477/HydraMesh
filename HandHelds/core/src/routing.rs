// SPDX-License-Identifier: LGPL-3.0-only
//! Portable shortest-path routing for self-healing — pick an alternate next hop
//! when a peer fails. Dijkstra over an explicit edge list (RTT weights), no
//! platform dependencies. Replaces the original tuple-keyed-map version, which
//! relied on `Hash` impls heapless does not provide for tuple keys.

use heapless::consts::*;
use heapless::{FnvIndexMap, String, Vec};

pub type Id = String<U32>;
/// Up to this many nodes / predecessor entries.
pub type Cap = U16;

#[derive(Clone, Debug)]
pub struct Edge {
    pub from: Id,
    pub to: Id,
    pub rtt: u32,
}

impl Edge {
    pub fn new(from: &str, to: &str, rtt: u32) -> Self {
        let mut f: Id = String::new();
        let mut t: Id = String::new();
        let _ = f.push_str(from);
        let _ = t.push_str(to);
        Edge { from: f, to: t, rtt }
    }
}

fn id_of(s: &str) -> Id {
    let mut id: Id = String::new();
    let _ = id.push_str(s);
    id
}

/// Dijkstra from `start`. Returns a predecessor map (`node -> previous node on the
/// shortest path from start`). `nodes` is the full node set; `edges` are directed.
pub fn dijkstra(nodes: &[Id], edges: &[Edge], start: &str) -> FnvIndexMap<Id, Id, Cap> {
    let mut dist: FnvIndexMap<Id, u32, Cap> = FnvIndexMap::new();
    let mut pred: FnvIndexMap<Id, Id, Cap> = FnvIndexMap::new();
    let mut visited: FnvIndexMap<Id, bool, Cap> = FnvIndexMap::new();

    for n in nodes {
        let _ = dist.insert(n.clone(), u32::MAX);
    }
    let start_id = id_of(start);
    let _ = dist.insert(start_id, 0);

    loop {
        // pick the unvisited node with the smallest tentative distance
        let mut u: Option<Id> = None;
        let mut best = u32::MAX;
        for n in nodes {
            if *visited.get(n).unwrap_or(&false) {
                continue;
            }
            let d = *dist.get(n).unwrap_or(&u32::MAX);
            if d <= best {
                best = d;
                u = Some(n.clone());
            }
        }
        let u = match u {
            Some(u) if best != u32::MAX => u,
            _ => break,
        };
        let _ = visited.insert(u.clone(), true);

        let du = *dist.get(&u).unwrap_or(&u32::MAX);
        for e in edges {
            if e.from != u {
                continue;
            }
            let alt = du.saturating_add(e.rtt);
            if alt < *dist.get(&e.to).unwrap_or(&u32::MAX) {
                let _ = dist.insert(e.to.clone(), alt);
                let _ = pred.insert(e.to.clone(), u.clone());
            }
        }
    }
    pred
}

/// Reconstruct the path start..=target from a predecessor map (target-first walk,
/// then reversed). Returns `None` if `target` is unreachable.
pub fn reconstruct_path(pred: &FnvIndexMap<Id, Id, Cap>, target: &str) -> Option<Vec<Id, U16>> {
    let mut path: Vec<Id, U16> = Vec::new();
    let mut cur = id_of(target);
    let _ = path.push(cur.clone());
    while let Some(p) = pred.get(&cur) {
        let p = p.clone();
        path.push(p.clone()).ok()?;
        cur = p;
    }
    // reverse in place
    let n = path.len();
    for i in 0..n / 2 {
        path.swap(i, n - 1 - i);
    }
    Some(path)
}

/// Convenience: the next hop from `start` toward `target` (the node right after
/// `start` on the shortest path), used by the heal routine.
pub fn next_hop(pred: &FnvIndexMap<Id, Id, Cap>, start: &str, target: &str) -> Option<Id> {
    let path = reconstruct_path(pred, target)?;
    if path.len() < 2 {
        return None;
    }
    if path[0] != id_of(start) {
        return None; // target not reachable from start
    }
    Some(path[1].clone())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn nodes(names: &[&str]) -> Vec<Id, U16> {
        let mut v: Vec<Id, U16> = Vec::new();
        for n in names {
            v.push(id_of(n)).unwrap();
        }
        v
    }

    #[test]
    fn direct_and_relayed_paths() {
        // a -> b (10), a -> c (1), c -> b (1): shortest a..b is via c.
        let ns = nodes(&["a", "b", "c"]);
        let edges = [
            Edge::new("a", "b", 10),
            Edge::new("a", "c", 1),
            Edge::new("c", "b", 1),
        ];
        let pred = dijkstra(&ns, &edges, "a");
        let path = reconstruct_path(&pred, "b").unwrap();
        let got: heapless::Vec<&str, U16> = path.iter().map(|s| s.as_str()).collect();
        assert_eq!(&got[..], &["a", "c", "b"]);
        assert_eq!(next_hop(&pred, "a", "b").unwrap().as_str(), "c");
    }

    #[test]
    fn unreachable_target() {
        let ns = nodes(&["a", "b", "x"]);
        let edges = [Edge::new("a", "b", 1)];
        let pred = dijkstra(&ns, &edges, "a");
        // x has no predecessor and isn't start -> not reachable from a
        assert!(next_hop(&pred, "a", "x").is_none());
    }

    #[test]
    fn heal_picks_alternate_hop() {
        // failed direct peer "b"; alternate route a->c->b means heal to "c".
        let ns = nodes(&["a", "b", "c"]);
        let edges = [Edge::new("a", "c", 5), Edge::new("c", "b", 5)];
        let pred = dijkstra(&ns, &edges, "a");
        assert_eq!(next_hop(&pred, "a", "b").unwrap().as_str(), "c");
    }
}
