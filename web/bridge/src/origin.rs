// SPDX-License-Identifier: LGPL-3.0-only
// Copyright (c) 2026 DeMoD LLC.
//
//! Which web pages may open a bridge connection.
//!
//! The browser does not apply its same-origin rule to WebSockets. Any page the
//! user visits can open `ws://127.0.0.1:7000`, so a bridge that accepts every
//! handshake hands every website a UDP socket on the user's machine. The
//! handshake's `Origin` header is the only thing that says which page is
//! asking, so it is checked here.
//!
//! - **No `Origin` header.** Admitted. That is a non-browser client, and such
//!   a client could send UDP itself.
//! - **A loopback page.** `http` or `https` on `localhost`, `127.0.0.1` or
//!   `[::1]`, any port, is admitted. That covers the web client served
//!   locally.
//! - **Anything else.** Admitted only if named with `--allow-origin`.
//!
//! That includes `null`, which is what a `file://` page sends. It is also what
//! a sandboxed iframe on any website sends, so admitting it by default would
//! reopen the hole for every site. A user who opens the client from `file://`
//! passes `--allow-origin null`, knowingly.

/// True if a handshake carrying `origin` may connect, given the operator's
/// `--allow-origin` list.
pub fn allowed(origin: Option<&str>, extra: &[String]) -> bool {
    let origin = match origin {
        None => return true,
        Some(o) => o.trim(),
    };
    if extra.iter().any(|e| e.trim().eq_ignore_ascii_case(origin)) {
        return true;
    }
    is_loopback(origin)
}

/// `http(s)://` + a loopback host + an optional numeric port, and nothing
/// else: no path, no userinfo, no other host that merely starts the same way.
fn is_loopback(origin: &str) -> bool {
    let lower = origin.to_ascii_lowercase();
    let rest = match lower
        .strip_prefix("http://")
        .or_else(|| lower.strip_prefix("https://"))
    {
        Some(r) => r,
        None => return false,
    };
    let (host, port) = if let Some(r) = rest.strip_prefix('[') {
        match r.split_once(']') {
            Some((h, p)) => (format!("[{h}]"), p),
            None => return false,
        }
    } else {
        let h = rest.split(':').next().unwrap_or("");
        (h.to_string(), &rest[h.len()..])
    };
    let port_ok = port.is_empty()
        || port
            .strip_prefix(':')
            .is_some_and(|p| !p.is_empty() && p.len() <= 5 && p.bytes().all(|b| b.is_ascii_digit()));
    port_ok && matches!(host.as_str(), "localhost" | "127.0.0.1" | "[::1]")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ok(o: &str) -> bool {
        allowed(Some(o), &[])
    }

    #[test]
    fn no_origin_is_a_non_browser_client() {
        assert!(allowed(None, &[]));
    }

    #[test]
    fn loopback_pages_are_admitted() {
        for o in [
            "http://localhost",
            "http://localhost:5173",
            "https://127.0.0.1:8443",
            "http://[::1]:7000",
            "HTTP://LOCALHOST:80",
        ] {
            assert!(ok(o), "{o}");
        }
    }

    #[test]
    fn other_sites_are_refused() {
        for o in [
            "https://example.com",
            "http://localhost.evil.example",
            "http://127.0.0.1.evil.example",
            "http://localhost@evil.example",
            "http://evil.example#localhost",
            "http://localhost:80/path",
            "http://localhost:",
            "http://localhost:123456",
            "http://[::1",
            "ws://localhost",
            "file://",
            "",
        ] {
            assert!(!ok(o), "{o}");
        }
    }

    #[test]
    fn null_needs_an_explicit_allow() {
        assert!(!ok("null"));
        assert!(allowed(Some("null"), &["null".to_string()]));
    }

    #[test]
    fn an_allowed_origin_is_matched_whole() {
        let extra = vec!["https://mesh.example".to_string()];
        assert!(allowed(Some("https://mesh.example"), &extra));
        assert!(!allowed(Some("https://mesh.example.evil"), &extra));
        assert!(!allowed(Some("https://evil.example"), &extra));
    }
}
