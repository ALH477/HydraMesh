// SPDX-License-Identifier: LGPL-3.0-only
//! Portable, host-testable core of the DCF handheld framework (Nintendo DSi /
//! Sony PSP). Everything here is platform-independent `no_std + alloc` and is
//! unit-tested on the host with plain `cargo test` — no devkitPro, no nightly.
//!
//! - [`wire`]    — certified 17-byte DeModFrame + 32-byte SuperPack quantum.
//! - [`proto`]   — the app message envelope (de)serialiser.
//! - [`routing`] — Dijkstra self-healing route selection.
//! - [`streamdb`]— reverse-trie path index (path -> document id).
//!
//! The platform binary crate (`../`, `dcf-framework`) provides only the DSi/PSP
//! glue (WiFi/storage/GUI FFI) on top of this verified core.
#![cfg_attr(not(test), no_std)]

extern crate alloc;

pub mod proto;
pub mod routing;
pub mod streamdb;
pub mod wire;
