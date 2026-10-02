// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//
//! dcf-ws-bridge's two decisions, kept out of `main.rs` so they can be tested:
//! which datagrams may cross ([`gate`]) and which web pages may connect
//! ([`origin`]).

pub mod gate;
pub mod origin;
