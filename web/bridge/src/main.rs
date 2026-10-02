// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//
//! dcf-ws-bridge — a minimal WebSocket↔UDP relay.
//!
//! A browser can't open raw UDP, so this bridges one WebSocket connection to one
//! UDP socket on the DCF mesh. The DCF codec runs in the browser (WASM); the
//! bridge does not decode, packetize or route. It does two checks:
//!
//!   - **Every datagram is gated, both ways** (`dcf_ws_bridge::gate`). Only a
//!     valid 17-byte DeModFrame or 32-byte SuperPack crosses; anything else is
//!     dropped and counted. The gate is Exsecutor's certified `custos`,
//!     compiled to C (`custos/`). Without it, the bridge was a general UDP
//!     sender: any bytes, to any host:port it was told about.
//!   - **Only allowed pages connect** (`dcf_ws_bridge::origin`). Browsers do
//!     not apply the same-origin rule to WebSockets, so the handshake's
//!     `Origin` is checked. Loopback pages and non-browser clients are
//!     admitted. Any other origin, `null` (`file://`) included, must be named
//!     with `--allow-origin`.
//!
//!   browser → bridge  text  WS frame = JSON control:
//!                            {"op":"addpeer","host":"127.0.0.1","port":7801}
//!                            {"op":"clear"}
//!   browser → bridge  binary WS frame = one UDP payload, sent to every peer
//!   mesh    → bridge  UDP datagram    = forwarded to the browser as a binary WS frame
//!
//! The wire is plaintext by design (export compliance) — run this behind WireGuard
//! or an operator-supplied tunnel. See Documentation/DCF_SECURITY_EXPOSURE.md.
//!
//! Usage: dcf-ws-bridge [--listen 127.0.0.1:7000] [--udp-bind 0.0.0.0:0]
//!                      [--allow-origin ORIGIN]...

use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use dcf_ws_bridge::{gate, origin};

use futures_util::{SinkExt, StreamExt};
use serde::Deserialize;
use tokio::net::{TcpListener, UdpSocket};
use tokio::sync::Mutex;
use tokio_tungstenite::tungstenite::handshake::server::{ErrorResponse, Request, Response};
use tokio_tungstenite::tungstenite::http;
use tokio_tungstenite::tungstenite::Message;

#[derive(Deserialize)]
#[serde(tag = "op", rename_all = "lowercase")]
enum Control {
    /// Register a mesh peer to broadcast outbound datagrams to.
    AddPeer { host: String, port: u16 },
    /// Forget all peers.
    Clear,
}

#[tokio::main]
async fn main() {
    let mut listen = "127.0.0.1:7000".to_string();
    let mut udp_bind = "0.0.0.0:0".to_string();
    let mut allow_origins: Vec<String> = Vec::new();
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "--listen" => listen = args.next().unwrap_or(listen),
            "--udp-bind" => udp_bind = args.next().unwrap_or(udp_bind),
            "--allow-origin" => match args.next() {
                Some(o) => allow_origins.push(o),
                None => {
                    eprintln!("--allow-origin needs a value (an origin, or `null` for file://)");
                    std::process::exit(2);
                }
            },
            "-h" | "--help" => {
                eprintln!(
                    "dcf-ws-bridge [--listen host:port] [--udp-bind host:port] \
                     [--allow-origin ORIGIN]...\n  \
                     Loopback pages and non-browser clients may connect; any other \
                     origin must be\n  named. `--allow-origin null` admits file:// \
                     pages -- and sandboxed iframes on any site."
                );
                return;
            }
            other => eprintln!("ignoring unknown arg: {other}"),
        }
    }

    let server = TcpListener::bind(&listen)
        .await
        .unwrap_or_else(|e| panic!("bind {listen}: {e}"));
    eprintln!("dcf-ws-bridge listening on ws://{listen}  (udp-bind {udp_bind})");
    eprintln!("reminder: the DCF wire is plaintext — run this behind WireGuard.");
    eprintln!("gate: only valid DeModFrames / SuperPacks are relayed, both directions.");
    if !allow_origins.is_empty() {
        eprintln!("origins admitted besides loopback: {}", allow_origins.join(", "));
    }
    let allow_origins = Arc::new(allow_origins);

    loop {
        let (stream, who) = match server.accept().await {
            Ok(v) => v,
            Err(e) => {
                eprintln!("accept: {e}");
                continue;
            }
        };
        let udp_bind = udp_bind.clone();
        let allow_origins = allow_origins.clone();
        tokio::spawn(async move {
            if let Err(e) = handle(stream, &udp_bind, &allow_origins).await {
                eprintln!("conn {who}: {e}");
            }
        });
    }
}

/// Refuse the handshake (HTTP 403) unless its `Origin` is admitted.
// The error type is tungstenite's callback contract, not a choice made here.
#[allow(clippy::result_large_err)]
fn check_origin(
    allow: &[String],
) -> impl FnOnce(&Request, Response) -> Result<Response, ErrorResponse> + '_ {
    move |req: &Request, resp: Response| {
        let o = req
            .headers()
            .get(http::header::ORIGIN)
            .map(|v| v.to_str().unwrap_or("\u{fffd}"));
        if origin::allowed(o, allow) {
            return Ok(resp);
        }
        let mut refusal = ErrorResponse::new(Some(
            "dcf-ws-bridge: origin not allowed (see --allow-origin)".to_string(),
        ));
        *refusal.status_mut() = http::StatusCode::FORBIDDEN;
        Err(refusal)
    }
}

async fn handle(
    stream: tokio::net::TcpStream,
    udp_bind: &str,
    allow_origins: &[String],
) -> Result<(), String> {
    let ws = tokio_tungstenite::accept_hdr_async(stream, check_origin(allow_origins))
        .await
        .map_err(|e| format!("ws handshake: {e}"))?;
    let (mut ws_tx, mut ws_rx) = ws.split();

    let udp = Arc::new(
        UdpSocket::bind(udp_bind)
            .await
            .map_err(|e| format!("udp bind {udp_bind}: {e}"))?,
    );
    let peers: Arc<Mutex<HashMap<SocketAddr, ()>>> = Arc::new(Mutex::new(HashMap::new()));
    let refused_in = Arc::new(AtomicU64::new(0));
    let mut refused_out: u64 = 0;

    // UDP → WS: forward each inbound datagram from a known peer to the browser.
    let (to_ws_tx, mut to_ws_rx) = tokio::sync::mpsc::channel::<Vec<u8>>(256);
    {
        let udp = udp.clone();
        let peers = peers.clone();
        let refused_in = refused_in.clone();
        tokio::spawn(async move {
            let mut buf = vec![0u8; 2048];
            loop {
                match udp.recv_from(&mut buf).await {
                    Ok((n, from)) => {
                        if !peers.lock().await.contains_key(&from) {
                            continue;
                        }
                        if !gate::admit(&buf[..n]).admitted() {
                            refused_in.fetch_add(1, Ordering::Relaxed);
                            continue;
                        }
                        if to_ws_tx.send(buf[..n].to_vec()).await.is_err() {
                            break;
                        }
                    }
                    Err(_) => break,
                }
            }
        });
    }

    loop {
        tokio::select! {
            // outbound: browser → mesh
            msg = ws_rx.next() => {
                let msg = match msg {
                    Some(Ok(m)) => m,
                    _ => break, // closed or errored
                };
                match msg {
                    Message::Binary(data) => {
                        if !gate::admit(&data).admitted() {
                            refused_out += 1;
                            continue;
                        }
                        let targets: Vec<SocketAddr> = peers.lock().await.keys().copied().collect();
                        for t in targets {
                            let _ = udp.send_to(&data, t).await;
                        }
                    }
                    Message::Text(txt) => {
                        match serde_json::from_str::<Control>(&txt) {
                            Ok(Control::AddPeer { host, port }) => {
                                match tokio::net::lookup_host((host.as_str(), port)).await {
                                    Ok(addrs) => {
                                        let mut p = peers.lock().await;
                                        for a in addrs { p.insert(a, ()); }
                                    }
                                    Err(e) => eprintln!("resolve {host}:{port}: {e}"),
                                }
                            }
                            Ok(Control::Clear) => peers.lock().await.clear(),
                            Err(e) => eprintln!("bad control json: {e}"),
                        }
                    }
                    Message::Ping(p) => { let _ = ws_tx.send(Message::Pong(p)).await; }
                    Message::Close(_) => break,
                    _ => {}
                }
            }
            // inbound: mesh → browser
            data = to_ws_rx.recv() => {
                match data {
                    Some(d) => {
                        if ws_tx.send(Message::Binary(d)).await.is_err() { break; }
                    }
                    None => break,
                }
            }
        }
    }
    let refused_in = refused_in.load(Ordering::Relaxed);
    if refused_out + refused_in > 0 {
        eprintln!("gate refused {refused_out} datagram(s) from the browser, {refused_in} from the mesh");
    }
    Ok(())
}
