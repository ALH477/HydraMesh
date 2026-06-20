// SPDX-License-Identifier: LGPL-3.0-only
//! Portable reverse-trie path index for StreamDB (path -> document id), no platform
//! deps. The original stored child nodes *inline* in a `FnvIndexMap<char, Node>`,
//! an infinitely-sized recursive type that cannot compile; children are boxed here.

use alloc::boxed::Box;
use heapless::consts::*;
use heapless::{FnvIndexMap, String, Vec};

pub type Path = String<U32>;
/// Maximum number of results returned by [`PathTrie::search`].
pub type SearchCap = U8;

#[derive(Default)]
pub struct TrieNode {
    document_id: Option<u128>,
    children: FnvIndexMap<char, Box<TrieNode>, U16>,
}

#[derive(Default)]
pub struct PathTrie {
    root: TrieNode,
}

fn valid_path(path: &str) -> bool {
    !(path.is_empty() || path.len() > 32 || path.contains('\0') || path.contains("//"))
}

impl PathTrie {
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert `path -> id`. Keys are stored reversed (so suffix queries are cheap,
    /// matching the original "reverse trie"). Returns false on an invalid path or
    /// when a fixed-capacity child map is full.
    pub fn insert(&mut self, path: &str, id: u128) -> bool {
        if !valid_path(path) {
            return false;
        }
        let mut node = &mut self.root;
        for ch in path.chars().rev() {
            if !node.children.contains_key(&ch) {
                if node.children.insert(ch, Box::new(TrieNode::default())).is_err() {
                    return false;
                }
            }
            node = node.children.get_mut(&ch).unwrap();
        }
        node.document_id = Some(id);
        true
    }

    /// Look up the document id for an exact path.
    pub fn get(&self, path: &str) -> Option<u128> {
        if !valid_path(path) {
            return None;
        }
        let mut node = &self.root;
        for ch in path.chars().rev() {
            node = node.children.get(&ch)?;
        }
        node.document_id
    }

    /// Remove a path. Returns true iff it was present.
    pub fn delete(&mut self, path: &str) -> bool {
        if self.get(path).is_none() {
            return false;
        }
        let mut node = &mut self.root;
        for ch in path.chars().rev() {
            node = node.children.get_mut(&ch).unwrap();
        }
        node.document_id = None;
        true
    }

    /// True iff any stored path ends with `suffix` (i.e. some key has this suffix);
    /// because keys are reversed, that's a prefix walk from the root.
    pub fn any_with_suffix(&self, suffix: &str) -> bool {
        let mut node = &self.root;
        for ch in suffix.chars().rev() {
            match node.children.get(&ch) {
                Some(n) => node = n,
                None => return false,
            }
        }
        true
    }

    /// Every stored path that begins with `prefix` (up to [`SearchCap`] results).
    /// Replaces the original `trie_search` + `trie_collect_paths`.
    pub fn search(&self, prefix: &str) -> Vec<Path, SearchCap> {
        let mut out: Vec<Path, SearchCap> = Vec::new();
        let mut acc: Vec<char, U32> = Vec::new();
        Self::collect(&self.root, &mut acc, prefix, &mut out);
        out
    }

    fn collect(
        node: &TrieNode,
        acc: &mut Vec<char, U32>,
        prefix: &str,
        out: &mut Vec<Path, SearchCap>,
    ) {
        if node.document_id.is_some() {
            // `acc` holds the reversed-key chars (root->node); the original path is
            // their reverse.
            let mut p: Path = String::new();
            let mut ok = true;
            for &c in acc.iter().rev() {
                if p.push(c).is_err() {
                    ok = false;
                    break;
                }
            }
            if ok && p.as_str().starts_with(prefix) {
                let _ = out.push(p);
            }
        }
        for (ch, child) in &node.children {
            if acc.push(*ch).is_err() {
                continue;
            }
            Self::collect(&**child, acc, prefix, out);
            acc.pop();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_get_delete() {
        let mut t = PathTrie::new();
        assert!(t.insert("/config", 1));
        assert!(t.insert("/messages/received", 2));
        assert!(t.insert("/peers/host", 3));
        assert_eq!(t.get("/config"), Some(1));
        assert_eq!(t.get("/messages/received"), Some(2));
        assert_eq!(t.get("/peers/host"), Some(3));
        assert_eq!(t.get("/missing"), None);
        assert!(t.delete("/config"));
        assert_eq!(t.get("/config"), None);
        assert!(!t.delete("/config")); // already gone
    }

    #[test]
    fn rejects_bad_paths() {
        let mut t = PathTrie::new();
        assert!(!t.insert("", 1));
        assert!(!t.insert("a//b", 1));
        let mut too_long: heapless::String<U64> = heapless::String::new();
        for _ in 0..33 {
            too_long.push('x').unwrap();
        }
        assert!(!t.insert(&too_long, 1));
    }

    #[test]
    fn suffix_query() {
        let mut t = PathTrie::new();
        t.insert("/peers/host", 1);
        t.insert("/peers/peer2", 2);
        assert!(t.any_with_suffix("host"));
        assert!(t.any_with_suffix("/peers/peer2"));
        assert!(!t.any_with_suffix("nope"));
    }

    #[test]
    fn prefix_search() {
        let mut t = PathTrie::new();
        t.insert("/peers/host", 1);
        t.insert("/peers/peer2", 2);
        t.insert("/config", 3);
        let found = t.search("/peers/");
        let mut peers: heapless::Vec<&str, U8> = found.iter().map(|p| p.as_str()).collect();
        peers.sort_unstable();
        assert_eq!(&peers[..], &["/peers/host", "/peers/peer2"]);
        let cfg = t.search("/config");
        assert_eq!(cfg.len(), 1);
        assert_eq!(cfg[0].as_str(), "/config");
        assert_eq!(t.search("/none").len(), 0);
    }
}
