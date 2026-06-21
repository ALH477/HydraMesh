// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//
//! dcf-ws-bridge — a minimal WebSocket↔UDP relay.
//!
//! A browser can't open raw UDP, so this bridges one WebSocket connection to one
//! UDP socket on the DCF mesh. It is deliberately dumb: it never parses a frame.
//! The DCF codec runs in the browser (WASM); this only shuttles opaque datagrams.
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

use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::Arc;

use futures_util::{SinkExt, StreamExt};
use serde::Deserialize;
use tokio::net::{TcpListener, UdpSocket};
use tokio::sync::Mutex;
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
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "--listen" => listen = args.next().unwrap_or(listen),
            "--udp-bind" => udp_bind = args.next().unwrap_or(udp_bind),
            "-h" | "--help" => {
                eprintln!("dcf-ws-bridge [--listen host:port] [--udp-bind host:port]");
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

    loop {
        let (stream, who) = match server.accept().await {
            Ok(v) => v,
            Err(e) => {
                eprintln!("accept: {e}");
                continue;
            }
        };
        let udp_bind = udp_bind.clone();
        tokio::spawn(async move {
            if let Err(e) = handle(stream, &udp_bind).await {
                eprintln!("conn {who}: {e}");
            }
        });
    }
}

async fn handle(stream: tokio::net::TcpStream, udp_bind: &str) -> Result<(), String> {
    let ws = tokio_tungstenite::accept_async(stream)
        .await
        .map_err(|e| format!("ws handshake: {e}"))?;
    let (mut ws_tx, mut ws_rx) = ws.split();

    let udp = Arc::new(
        UdpSocket::bind(udp_bind)
            .await
            .map_err(|e| format!("udp bind {udp_bind}: {e}"))?,
    );
    let peers: Arc<Mutex<HashMap<SocketAddr, ()>>> = Arc::new(Mutex::new(HashMap::new()));

    // UDP → WS: forward each inbound datagram from a known peer to the browser.
    let (to_ws_tx, mut to_ws_rx) = tokio::sync::mpsc::channel::<Vec<u8>>(256);
    {
        let udp = udp.clone();
        let peers = peers.clone();
        tokio::spawn(async move {
            let mut buf = vec![0u8; 2048];
            loop {
                match udp.recv_from(&mut buf).await {
                    Ok((n, from)) => {
                        if peers.lock().await.contains_key(&from)
                            && to_ws_tx.send(buf[..n].to_vec()).await.is_err()
                        {
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
    Ok(())
}
