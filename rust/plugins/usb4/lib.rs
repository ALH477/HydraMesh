//! Thunderbolt/USB4 Transport Plugin for DeMoD Communications Framework (DCF)
//! Rust Implementation - Production Version
//!
//! Copyright © 2025 DeMoD LLC
//!
//! This file is part of DeMoD Communications Framework (DCF).
//!
//! DCF is free software: you can redistribute it and/or modify
//! it under the terms of the GNU Lesser General Public License as published by
//! the Free Software Foundation, either version 3 of the License, or
//! (at your option) any later version.

use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::net::{Ipv6Addr, SocketAddrV6, TcpListener, TcpStream, UdpSocket};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use parking_lot::{Mutex, RwLock};
use thiserror::Error;
use tokio::sync::mpsc;

// ============================================================================
// Constants
// ============================================================================

/// Default UDP port for control messages
pub const DEFAULT_CONTROL_PORT: u16 = 5000;

/// Default TCP port for data streams
pub const DEFAULT_DATA_PORT: u16 = 5001;

/// Default socket buffer size (64 MiB)
pub const DEFAULT_BUFFER_SIZE: usize = 64 * 1024 * 1024;

/// Maximum UDP payload size
pub const MAX_UDP_PAYLOAD: usize = 65507;

/// Connection backlog for TCP listener
pub const CONNECTION_BACKLOG: i32 = 128;

/// RTT Ping magic number (big-endian): "RTTPT"
pub const RTT_PING_MAGIC: u64 = 0x5254545054;

/// RTT Pong magic number (big-endian): "RTTPS"
pub const RTT_PONG_MAGIC: u64 = 0x5254545053;

// ============================================================================
// Error Types
// ============================================================================

/// Transport-specific errors
#[derive(Error, Debug)]
pub enum TransportError {
    #[error("IO error: {0}")]
    Io(#[from] io::Error),

    #[error("Invalid IPv6 address: {0}")]
    InvalidAddress(String),

    #[error("Network interface not found: {0}")]
    InterfaceNotFound(String),

    #[error("Message too large: {size} bytes (max {max})")]
    MessageTooLarge { size: usize, max: usize },

    #[error("Connection failed: {0}")]
    ConnectionFailed(String),

    #[error("Transport not initialized")]
    NotInitialized,

    #[error("Transport shutdown")]
    Shutdown,

    #[error("Invalid message format: {0}")]
    InvalidMessage(String),

    #[error("Timeout")]
    Timeout,

    #[error("Socket option error: {0}")]
    SocketOption(String),
}

pub type Result<T> = std::result::Result<T, TransportError>;

// ============================================================================
// Message Types
// ============================================================================

/// Type of incoming message
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MessageType {
    /// UDP control message
    ControlMessage,
    /// TCP data stream
    DataStream,
}

/// Incoming message data
#[derive(Debug)]
pub enum MessageData {
    /// Raw bytes from UDP
    Bytes(Vec<u8>),
    /// TCP stream handle
    Stream(TcpStream),
}

// ============================================================================
// Statistics Tracking
// ============================================================================

/// Transport statistics (thread-safe)
#[derive(Debug, Default)]
pub struct TransportStatistics {
    messages_sent: AtomicU64,
    messages_received: AtomicU64,
    bytes_sent: AtomicU64,
    bytes_received: AtomicU64,
    connections_accepted: AtomicU64,
    connections_initiated: AtomicU64,
    errors: AtomicU64,
    last_reset: Mutex<Instant>,
}

impl TransportStatistics {
    pub fn new() -> Self {
        Self {
            last_reset: Mutex::new(Instant::now()),
            ..Default::default()
        }
    }

    pub fn record_sent(&self, bytes: usize) {
        self.messages_sent.fetch_add(1, Ordering::Relaxed);
        self.bytes_sent.fetch_add(bytes as u64, Ordering::Relaxed);
    }

    pub fn record_received(&self, bytes: usize) {
        self.messages_received.fetch_add(1, Ordering::Relaxed);
        self.bytes_received.fetch_add(bytes as u64, Ordering::Relaxed);
    }

    pub fn record_error(&self) {
        self.errors.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_connection(&self, initiated: bool) {
        if initiated {
            self.connections_initiated.fetch_add(1, Ordering::Relaxed);
        } else {
            self.connections_accepted.fetch_add(1, Ordering::Relaxed);
        }
    }

    pub fn get_stats(&self) -> Stats {
        Stats {
            messages_sent: self.messages_sent.load(Ordering::Relaxed),
            messages_received: self.messages_received.load(Ordering::Relaxed),
            bytes_sent: self.bytes_sent.load(Ordering::Relaxed),
            bytes_received: self.bytes_received.load(Ordering::Relaxed),
            connections_accepted: self.connections_accepted.load(Ordering::Relaxed),
            connections_initiated: self.connections_initiated.load(Ordering::Relaxed),
            errors: self.errors.load(Ordering::Relaxed),
            uptime_secs: self.last_reset.lock().elapsed().as_secs(),
        }
    }

    pub fn reset(&self) {
        self.messages_sent.store(0, Ordering::Relaxed);
        self.messages_received.store(0, Ordering::Relaxed);
        self.bytes_sent.store(0, Ordering::Relaxed);
        self.bytes_received.store(0, Ordering::Relaxed);
        self.connections_accepted.store(0, Ordering::Relaxed);
        self.connections_initiated.store(0, Ordering::Relaxed);
        self.errors.store(0, Ordering::Relaxed);
        *self.last_reset.lock() = Instant::now();
    }
}

/// Snapshot of transport statistics
#[derive(Debug, Clone, Default)]
pub struct Stats {
    pub messages_sent: u64,
    pub messages_received: u64,
    pub bytes_sent: u64,
    pub bytes_received: u64,
    pub connections_accepted: u64,
    pub connections_initiated: u64,
    pub errors: u64,
    pub uptime_secs: u64,
}

// ============================================================================
// Logger
// ============================================================================

/// Log level for transport messages
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogLevel {
    Debug,
    Info,
    Warn,
    Error,
}

/// Logger function type
pub type LoggerFn = Box<dyn Fn(LogLevel, &str) + Send + Sync>;

// ============================================================================
// Message Handler
// ============================================================================

/// Handler for incoming messages
pub type MessageHandlerFn = Box<dyn Fn(MessageType, MessageData) + Send + Sync>;

// ============================================================================
// Configuration
// ============================================================================

/// Thunderbolt transport configuration
#[derive(Debug, Clone)]
pub struct ThunderboltConfig {
    /// Network interface name (e.g., "thunderbolt0")
    pub interface: String,
    /// UDP port for control messages
    pub control_port: u16,
    /// TCP port for data streams
    pub data_port: u16,
    /// Socket buffer size
    pub buffer_size: usize,
    /// Enable TCP_NODELAY
    pub tcp_nodelay: bool,
    /// Enable SO_KEEPALIVE
    pub tcp_keepalive: bool,
}

impl Default for ThunderboltConfig {
    fn default() -> Self {
        Self {
            interface: "thunderbolt0".to_string(),
            control_port: DEFAULT_CONTROL_PORT,
            data_port: DEFAULT_DATA_PORT,
            buffer_size: DEFAULT_BUFFER_SIZE,
            tcp_nodelay: true,
            tcp_keepalive: true,
        }
    }
}

// ============================================================================
// IPv6 Address Parsing
// ============================================================================

/// Parse IPv6 address with optional scope (e.g., "fe80::1%eth0" or "fe80::1%2")
fn parse_ipv6_with_scope(address: &str, default_scope: u32) -> Result<(Ipv6Addr, u32)> {
    if let Some(pos) = address.find('%') {
        let ip_str = &address[..pos];
        let scope_str = &address[pos + 1..];
        
        let ip: Ipv6Addr = ip_str.parse()
            .map_err(|_| TransportError::InvalidAddress(address.to_string()))?;
        
        // Try parsing as numeric scope ID first
        let scope_id = if let Ok(id) = scope_str.parse::<u32>() {
            id
        } else {
            // Try to resolve interface name to index
            get_interface_index(scope_str)?
        };
        
        Ok((ip, scope_id))
    } else {
        let ip: Ipv6Addr = address.parse()
            .map_err(|_| TransportError::InvalidAddress(address.to_string()))?;
        Ok((ip, default_scope))
    }
}

/// Get network interface index by name
#[cfg(target_os = "linux")]
fn get_interface_index(name: &str) -> Result<u32> {
    use std::ffi::CString;
    
    let c_name = CString::new(name)
        .map_err(|_| TransportError::InterfaceNotFound(name.to_string()))?;
    
    let index = unsafe { libc::if_nametoindex(c_name.as_ptr()) };
    
    if index == 0 {
        Err(TransportError::InterfaceNotFound(name.to_string()))
    } else {
        Ok(index)
    }
}

#[cfg(not(target_os = "linux"))]
fn get_interface_index(name: &str) -> Result<u32> {
    // Fallback for non-Linux systems
    name.parse::<u32>()
        .map_err(|_| TransportError::InterfaceNotFound(name.to_string()))
}

// ============================================================================
// Socket Configuration
// ============================================================================

/// Bind socket to specific network interface (Linux only)
#[cfg(target_os = "linux")]
fn bind_to_device(socket: &socket2::Socket, interface: &str) -> Result<()> {
    use std::ffi::CString;
    use std::os::unix::io::AsRawFd;
    
    let c_name = CString::new(interface)
        .map_err(|_| TransportError::InterfaceNotFound(interface.to_string()))?;
    
    let result = unsafe {
        libc::setsockopt(
            socket.as_raw_fd(),
            libc::SOL_SOCKET,
            libc::SO_BINDTODEVICE,
            c_name.as_ptr() as *const libc::c_void,
            (interface.len() + 1) as libc::socklen_t,
        )
    };
    
    if result < 0 {
        Err(TransportError::SocketOption(format!(
            "Failed to bind to device {}: {}",
            interface,
            io::Error::last_os_error()
        )))
    } else {
        Ok(())
    }
}

#[cfg(not(target_os = "linux"))]
fn bind_to_device(_socket: &socket2::Socket, _interface: &str) -> Result<()> {
    // SO_BINDTODEVICE is Linux-specific
    Ok(())
}

/// Configure TCP socket for optimal performance
fn configure_tcp_socket(socket: &socket2::Socket, config: &ThunderboltConfig) -> Result<()> {
    use socket2::TcpKeepalive;
    
    // Set buffer sizes
    socket.set_send_buffer_size(config.buffer_size)
        .map_err(|e| TransportError::SocketOption(e.to_string()))?;
    socket.set_recv_buffer_size(config.buffer_size)
        .map_err(|e| TransportError::SocketOption(e.to_string()))?;
    
    // TCP_NODELAY for low latency
    if config.tcp_nodelay {
        socket.set_nodelay(true)
            .map_err(|e| TransportError::SocketOption(e.to_string()))?;
    }
    
    // SO_KEEPALIVE for connection health
    if config.tcp_keepalive {
        let keepalive = TcpKeepalive::new()
            .with_time(Duration::from_secs(60));
        socket.set_tcp_keepalive(&keepalive)
            .map_err(|e| TransportError::SocketOption(e.to_string()))?;
    }
    
    // Non-blocking mode
    socket.set_nonblocking(true)
        .map_err(|e| TransportError::SocketOption(e.to_string()))?;
    
    Ok(())
}

// ============================================================================
// RTT Measurement Protocol
// ============================================================================

/// Pack RTT ping message: magic (8 bytes) + timestamp (8 bytes)
fn pack_rtt_ping(timestamp_ns: u64) -> [u8; 16] {
    let mut msg = [0u8; 16];
    msg[0..8].copy_from_slice(&RTT_PING_MAGIC.to_be_bytes());
    msg[8..16].copy_from_slice(&timestamp_ns.to_be_bytes());
    msg
}

/// Pack RTT pong message: magic (8 bytes) + timestamp (8 bytes)
fn pack_rtt_pong(timestamp_ns: u64) -> [u8; 16] {
    let mut msg = [0u8; 16];
    msg[0..8].copy_from_slice(&RTT_PONG_MAGIC.to_be_bytes());
    msg[8..16].copy_from_slice(&timestamp_ns.to_be_bytes());
    msg
}

/// Unpack RTT pong message. Returns timestamp or None if invalid.
fn unpack_rtt_pong(msg: &[u8]) -> Option<u64> {
    if msg.len() != 16 {
        return None;
    }
    
    let magic = u64::from_be_bytes([
        msg[0], msg[1], msg[2], msg[3],
        msg[4], msg[5], msg[6], msg[7],
    ]);
    
    if magic != RTT_PONG_MAGIC {
        return None;
    }
    
    let timestamp = u64::from_be_bytes([
        msg[8], msg[9], msg[10], msg[11],
        msg[12], msg[13], msg[14], msg[15],
    ]);
    
    Some(timestamp)
}

/// Unpack RTT ping message. Returns timestamp or None if invalid.
fn unpack_rtt_ping(msg: &[u8]) -> Option<u64> {
    if msg.len() != 16 {
        return None;
    }
    
    let magic = u64::from_be_bytes([
        msg[0], msg[1], msg[2], msg[3],
        msg[4], msg[5], msg[6], msg[7],
    ]);
    
    if magic != RTT_PING_MAGIC {
        return None;
    }
    
    let timestamp = u64::from_be_bytes([
        msg[8], msg[9], msg[10], msg[11],
        msg[12], msg[13], msg[14], msg[15],
    ]);
    
    Some(timestamp)
}

/// Get current time in nanoseconds
fn get_nanosecond_timestamp() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0)
}

// ============================================================================
// Thunderbolt Transport
// ============================================================================

/// Thunderbolt/USB4 transport layer using IPv6 link-local addresses.
/// Provides UDP control channel and TCP data streams for high-performance communication.
pub struct ThunderboltTransport {
    config: ThunderboltConfig,
    interface_index: u32,
    udp_socket: Arc<UdpSocket>,
    tcp_listener: Arc<TcpListener>,
    shutdown_flag: Arc<AtomicBool>,
    message_handler: Arc<RwLock<Option<MessageHandlerFn>>>,
    logger: Arc<RwLock<Option<LoggerFn>>>,
    stats: Arc<TransportStatistics>,
    // Thread handles stored for cleanup
    acceptor_handle: Mutex<Option<std::thread::JoinHandle<()>>>,
    receiver_handle: Mutex<Option<std::thread::JoinHandle<()>>>,
}

impl ThunderboltTransport {
    /// Create a new Thunderbolt transport instance.
    ///
    /// # Arguments
    /// * `config` - Transport configuration
    /// * `message_handler` - Optional callback for incoming messages
    /// * `logger` - Optional logging function
    ///
    /// # Example
    /// ```ignore
    /// let transport = ThunderboltTransport::new(
    ///     ThunderboltConfig {
    ///         interface: "thunderbolt0".to_string(),
    ///         ..Default::default()
    ///     },
    ///     Some(Box::new(|msg_type, data| {
    ///         println!("Received: {:?}", msg_type);
    ///     })),
    ///     Some(Box::new(|level, msg| {
    ///         println!("[{:?}] {}", level, msg);
    ///     })),
    /// )?;
    /// ```
    pub fn new(
        config: ThunderboltConfig,
        message_handler: Option<MessageHandlerFn>,
        logger: Option<LoggerFn>,
    ) -> Result<Arc<Self>> {
        // Get interface index
        let interface_index = get_interface_index(&config.interface)?;
        
        // Create UDP socket
        let udp_socket = Self::create_udp_socket(&config, interface_index)?;
        
        // Create TCP listener
        let tcp_listener = Self::create_tcp_listener(&config, interface_index)?;
        
        let transport = Arc::new(Self {
            config: config.clone(),
            interface_index,
            udp_socket: Arc::new(udp_socket),
            tcp_listener: Arc::new(tcp_listener),
            shutdown_flag: Arc::new(AtomicBool::new(false)),
            message_handler: Arc::new(RwLock::new(message_handler)),
            logger: Arc::new(RwLock::new(logger)),
            stats: Arc::new(TransportStatistics::new()),
            acceptor_handle: Mutex::new(None),
            receiver_handle: Mutex::new(None),
        });
        
        transport.log(LogLevel::Info, &format!(
            "Initialized interface {} (index {})",
            config.interface, interface_index
        ));
        
        transport.log(LogLevel::Info, &format!(
            "UDP socket bound to port {} (non-blocking)",
            config.control_port
        ));
        
        transport.log(LogLevel::Info, &format!(
            "TCP listener bound to port {}",
            config.data_port
        ));
        
        // Start background threads
        transport.start_background_threads();
        
        Ok(transport)
    }

    fn create_udp_socket(config: &ThunderboltConfig, _interface_index: u32) -> Result<UdpSocket> {
        use socket2::{Domain, Protocol, Socket, Type};
        
        let socket = Socket::new(Domain::IPV6, Type::DGRAM, Some(Protocol::UDP))
            .map_err(TransportError::Io)?;
        
        // Enable address reuse
        socket.set_reuse_address(true)
            .map_err(|e| TransportError::SocketOption(e.to_string()))?;
        
        // Bind to interface
        bind_to_device(&socket, &config.interface)?;
        
        // Set non-blocking
        socket.set_nonblocking(true)
            .map_err(|e| TransportError::SocketOption(e.to_string()))?;
        
        // Bind to port (all IPv6 addresses)
        let addr = SocketAddrV6::new(Ipv6Addr::UNSPECIFIED, config.control_port, 0, 0);
        socket.bind(&addr.into())
            .map_err(TransportError::Io)?;
        
        Ok(socket.into())
    }

    fn create_tcp_listener(config: &ThunderboltConfig, _interface_index: u32) -> Result<TcpListener> {
        use socket2::{Domain, Protocol, Socket, Type};
        
        let socket = Socket::new(Domain::IPV6, Type::STREAM, Some(Protocol::TCP))
            .map_err(TransportError::Io)?;
        
        // Enable address reuse
        socket.set_reuse_address(true)
            .map_err(|e| TransportError::SocketOption(e.to_string()))?;
        
        // Bind to interface
        bind_to_device(&socket, &config.interface)?;
        
        // Bind to port
        let addr = SocketAddrV6::new(Ipv6Addr::UNSPECIFIED, config.data_port, 0, 0);
        socket.bind(&addr.into())
            .map_err(TransportError::Io)?;
        
        // Listen
        socket.listen(CONNECTION_BACKLOG)
            .map_err(TransportError::Io)?;
        
        // Set non-blocking for accept
        socket.set_nonblocking(true)
            .map_err(|e| TransportError::SocketOption(e.to_string()))?;
        
        Ok(socket.into())
    }

    fn start_background_threads(self: &Arc<Self>) {
        // TCP Acceptor thread
        let transport = Arc::clone(self);
        let acceptor = std::thread::Builder::new()
            .name("Thunderbolt-TCP-Acceptor".to_string())
            .spawn(move || {
                transport.tcp_acceptor_loop();
            })
            .expect("Failed to spawn TCP acceptor thread");
        
        *self.acceptor_handle.lock() = Some(acceptor);
        
        // UDP Receiver thread
        let transport = Arc::clone(self);
        let receiver = std::thread::Builder::new()
            .name("Thunderbolt-UDP-Receiver".to_string())
            .spawn(move || {
                transport.udp_receiver_loop();
            })
            .expect("Failed to spawn UDP receiver thread");
        
        *self.receiver_handle.lock() = Some(receiver);
        
        self.log(LogLevel::Info, "Background threads started");
    }

    fn log(&self, level: LogLevel, message: &str) {
        if let Some(ref logger) = *self.logger.read() {
            logger(level, message);
        }
    }

    // ========================================================================
    // Sending Functions
    // ========================================================================

    /// Send small control message via UDP.
    ///
    /// # Arguments
    /// * `data` - Byte slice to send (max 65507 bytes)
    /// * `peer_address` - IPv6 address with optional scope (e.g., "fe80::1%thunderbolt0")
    pub fn send_message(&self, data: &[u8], peer_address: &str) -> Result<usize> {
        if data.len() > MAX_UDP_PAYLOAD {
            return Err(TransportError::MessageTooLarge {
                size: data.len(),
                max: MAX_UDP_PAYLOAD,
            });
        }
        
        let (ip, scope_id) = parse_ipv6_with_scope(peer_address, self.interface_index)?;
        let addr = SocketAddrV6::new(ip, self.config.control_port, 0, scope_id);
        
        let sent = self.udp_socket.send_to(data, addr)?;
        
        self.stats.record_sent(sent);
        self.log(LogLevel::Debug, &format!("Sent {} bytes to {}", sent, peer_address));
        
        Ok(sent)
    }

    /// Open a bidirectional TCP stream to peer for large transfers.
    ///
    /// # Arguments
    /// * `peer_address` - IPv6 address with optional scope
    ///
    /// # Returns
    /// A configured TCP stream ready for reading/writing
    pub fn open_data_stream(&self, peer_address: &str) -> Result<TcpStream> {
        use socket2::{Domain, Protocol, Socket, Type};
        
        let (ip, scope_id) = parse_ipv6_with_scope(peer_address, self.interface_index)?;
        
        let socket = Socket::new(Domain::IPV6, Type::STREAM, Some(Protocol::TCP))
            .map_err(TransportError::Io)?;
        
        // Bind to interface
        bind_to_device(&socket, &self.config.interface)?;
        
        // Configure for performance
        configure_tcp_socket(&socket, &self.config)?;
        
        // Connect to peer
        let addr = SocketAddrV6::new(ip, self.config.data_port, 0, scope_id);
        
        // For non-blocking connect, we need to handle EINPROGRESS
        socket.set_nonblocking(false)
            .map_err(|e| TransportError::SocketOption(e.to_string()))?;
        
        socket.connect_timeout(&addr.into(), Duration::from_secs(10))
            .map_err(|e| TransportError::ConnectionFailed(e.to_string()))?;
        
        socket.set_nonblocking(true)
            .map_err(|e| TransportError::SocketOption(e.to_string()))?;
        
        self.stats.record_connection(true);
        self.log(LogLevel::Info, &format!("Established data stream to {}", peer_address));
        
        Ok(socket.into())
    }

    // ========================================================================
    // RTT Measurement
    // ========================================================================

    /// Measure round-trip time to peer by sending UDP ping and waiting for response.
    ///
    /// Peer must echo packets with RTT_PING_MAGIC to RTT_PONG_MAGIC.
    ///
    /// # Arguments
    /// * `peer_address` - IPv6 address with optional scope
    /// * `timeout` - Maximum time to wait for response
    ///
    /// # Returns
    /// RTT in seconds, or None on timeout
    pub fn measure_rtt(&self, peer_address: &str, timeout: Duration) -> Result<Option<f64>> {
        let (ip, scope_id) = parse_ipv6_with_scope(peer_address, self.interface_index)?;
        let addr = SocketAddrV6::new(ip, self.config.control_port, 0, scope_id);
        
        // Send ping
        let send_time = get_nanosecond_timestamp();
        let ping_msg = pack_rtt_ping(send_time);
        
        self.udp_socket.send_to(&ping_msg, addr)?;
        
        // Wait for pong with timeout
        let deadline = Instant::now() + timeout;
        let mut buf = [0u8; 1024];
        
        while Instant::now() < deadline {
            match self.udp_socket.recv_from(&mut buf) {
                Ok((len, _from)) => {
                    if let Some(recv_timestamp) = unpack_rtt_pong(&buf[..len]) {
                        if recv_timestamp == send_time {
                            let recv_time = get_nanosecond_timestamp();
                            let rtt_ns = recv_time.saturating_sub(send_time);
                            let rtt_sec = rtt_ns as f64 / 1_000_000_000.0;
                            
                            self.log(LogLevel::Debug, &format!(
                                "RTT to {}: {:.6} ms",
                                peer_address, rtt_sec * 1000.0
                            ));
                            
                            return Ok(Some(rtt_sec));
                        }
                    }
                }
                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                    std::thread::sleep(Duration::from_micros(100));
                }
                Err(e) => {
                    self.log(LogLevel::Error, &format!("RTT pong receive failed: {}", e));
                    return Err(TransportError::Io(e));
                }
            }
        }
        
        self.log(LogLevel::Debug, "RTT measurement timed out");
        Ok(None)
    }

    // ========================================================================
    // Receiver Loops
    // ========================================================================

    fn tcp_acceptor_loop(&self) {
        self.log(LogLevel::Info, "TCP acceptor loop started");
        
        while !self.shutdown_flag.load(Ordering::Relaxed) {
            match self.tcp_listener.accept() {
                Ok((mut stream, addr)) => {
                    self.stats.record_connection(false);
                    self.log(LogLevel::Debug, &format!("Accepted connection from {}", addr));
                    
                    // Configure the accepted socket
                    if let Err(e) = stream.set_nodelay(self.config.tcp_nodelay) {
                        self.log(LogLevel::Warn, &format!("Failed to set TCP_NODELAY: {}", e));
                    }
                    
                    // Invoke message handler
                    if let Some(ref handler) = *self.message_handler.read() {
                        handler(MessageType::DataStream, MessageData::Stream(stream));
                    }
                }
                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                    std::thread::sleep(Duration::from_millis(10));
                }
                Err(e) => {
                    self.stats.record_error();
                    self.log(LogLevel::Error, &format!("Accept failed: {}", e));
                    std::thread::sleep(Duration::from_secs(1));
                }
            }
        }
        
        self.log(LogLevel::Info, "TCP acceptor loop shutting down");
    }

    fn udp_receiver_loop(&self) {
        self.log(LogLevel::Info, "UDP receiver loop started");
        
        let mut buf = vec![0u8; MAX_UDP_PAYLOAD];
        
        while !self.shutdown_flag.load(Ordering::Relaxed) {
            match self.udp_socket.recv_from(&mut buf) {
                Ok((len, from)) => {
                    let data = buf[..len].to_vec();
                    self.stats.record_received(len);
                    
                    // Check if this is an RTT ping and respond with pong
                    if let Some(timestamp) = unpack_rtt_ping(&data) {
                        let pong = pack_rtt_pong(timestamp);
                        let _ = self.udp_socket.send_to(&pong, from);
                        continue;
                    }
                    
                    self.log(LogLevel::Debug, &format!("Received {} bytes from {}", len, from));
                    
                    // Invoke message handler
                    if let Some(ref handler) = *self.message_handler.read() {
                        handler(MessageType::ControlMessage, MessageData::Bytes(data));
                    }
                }
                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                    std::thread::sleep(Duration::from_millis(10));
                }
                Err(e) => {
                    self.stats.record_error();
                    self.log(LogLevel::Error, &format!("UDP receive failed: {}", e));
                    std::thread::sleep(Duration::from_secs(1));
                }
            }
        }
        
        self.log(LogLevel::Info, "UDP receiver loop shutting down");
    }

    // ========================================================================
    // Statistics Access
    // ========================================================================

    /// Get current transport statistics.
    pub fn get_stats(&self) -> Stats {
        self.stats.get_stats()
    }

    /// Reset transport statistics.
    pub fn reset_stats(&self) {
        self.stats.reset();
        self.log(LogLevel::Info, "Statistics reset");
    }

    // ========================================================================
    // Shutdown
    // ========================================================================

    /// Gracefully shut down the transport layer.
    pub fn close(&self) {
        self.shutdown_flag.store(true, Ordering::SeqCst);
        
        // Give threads time to notice shutdown
        std::thread::sleep(Duration::from_millis(100));
        
        // Join threads
        if let Some(handle) = self.acceptor_handle.lock().take() {
            let _ = handle.join();
        }
        if let Some(handle) = self.receiver_handle.lock().take() {
            let _ = handle.join();
        }
        
        self.log(LogLevel::Info, "Transport closed");
    }

    /// Check if transport is running
    pub fn is_running(&self) -> bool {
        !self.shutdown_flag.load(Ordering::Relaxed)
    }

    /// Get the interface name
    pub fn interface(&self) -> &str {
        &self.config.interface
    }

    /// Get the control port
    pub fn control_port(&self) -> u16 {
        self.config.control_port
    }

    /// Get the data port
    pub fn data_port(&self) -> u16 {
        self.config.data_port
    }
}

impl Drop for ThunderboltTransport {
    fn drop(&mut self) {
        self.close();
    }
}

// ============================================================================
// Builder Pattern
// ============================================================================

/// Builder for ThunderboltTransport
pub struct ThunderboltTransportBuilder {
    config: ThunderboltConfig,
    message_handler: Option<MessageHandlerFn>,
    logger: Option<LoggerFn>,
}

impl ThunderboltTransportBuilder {
    pub fn new() -> Self {
        Self {
            config: ThunderboltConfig::default(),
            message_handler: None,
            logger: None,
        }
    }

    pub fn interface(mut self, interface: impl Into<String>) -> Self {
        self.config.interface = interface.into();
        self
    }

    pub fn control_port(mut self, port: u16) -> Self {
        self.config.control_port = port;
        self
    }

    pub fn data_port(mut self, port: u16) -> Self {
        self.config.data_port = port;
        self
    }

    pub fn buffer_size(mut self, size: usize) -> Self {
        self.config.buffer_size = size;
        self
    }

    pub fn tcp_nodelay(mut self, enabled: bool) -> Self {
        self.config.tcp_nodelay = enabled;
        self
    }

    pub fn tcp_keepalive(mut self, enabled: bool) -> Self {
        self.config.tcp_keepalive = enabled;
        self
    }

    pub fn message_handler<F>(mut self, handler: F) -> Self
    where
        F: Fn(MessageType, MessageData) + Send + Sync + 'static,
    {
        self.message_handler = Some(Box::new(handler));
        self
    }

    pub fn logger<F>(mut self, logger: F) -> Self
    where
        F: Fn(LogLevel, &str) + Send + Sync + 'static,
    {
        self.logger = Some(Box::new(logger));
        self
    }

    pub fn build(self) -> Result<Arc<ThunderboltTransport>> {
        ThunderboltTransport::new(self.config, self.message_handler, self.logger)
    }
}

impl Default for ThunderboltTransportBuilder {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Plugin Registration
// ============================================================================

/// Create a new Thunderbolt transport with the simplified API
pub fn make_thunderbolt_transport(
    interface: &str,
    control_port: u16,
    data_port: u16,
    message_handler: Option<MessageHandlerFn>,
    logger: Option<LoggerFn>,
) -> Result<Arc<ThunderboltTransport>> {
    ThunderboltTransport::new(
        ThunderboltConfig {
            interface: interface.to_string(),
            control_port,
            data_port,
            ..Default::default()
        },
        message_handler,
        logger,
    )
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ipv6_simple() {
        let (ip, scope) = parse_ipv6_with_scope("::1", 0).unwrap();
        assert_eq!(ip, Ipv6Addr::LOCALHOST);
        assert_eq!(scope, 0);
    }

    #[test]
    fn test_parse_ipv6_with_numeric_scope() {
        let (ip, scope) = parse_ipv6_with_scope("fe80::1%2", 0).unwrap();
        assert_eq!(ip, "fe80::1".parse::<Ipv6Addr>().unwrap());
        assert_eq!(scope, 2);
    }

    #[test]
    fn test_rtt_ping_pong_pack_unpack() {
        let timestamp = 1234567890123456789u64;
        
        let ping = pack_rtt_ping(timestamp);
        let unpacked = unpack_rtt_ping(&ping);
        assert_eq!(unpacked, Some(timestamp));
        
        let pong = pack_rtt_pong(timestamp);
        let unpacked = unpack_rtt_pong(&pong);
        assert_eq!(unpacked, Some(timestamp));
    }

    #[test]
    fn test_stats_tracking() {
        let stats = TransportStatistics::new();
        
        stats.record_sent(100);
        stats.record_received(50);
        stats.record_error();
        stats.record_connection(true);
        stats.record_connection(false);
        
        let snapshot = stats.get_stats();
        assert_eq!(snapshot.messages_sent, 1);
        assert_eq!(snapshot.bytes_sent, 100);
        assert_eq!(snapshot.messages_received, 1);
        assert_eq!(snapshot.bytes_received, 50);
        assert_eq!(snapshot.errors, 1);
        assert_eq!(snapshot.connections_initiated, 1);
        assert_eq!(snapshot.connections_accepted, 1);
    }

    #[test]
    fn test_stats_reset() {
        let stats = TransportStatistics::new();
        
        stats.record_sent(100);
        stats.reset();
        
        let snapshot = stats.get_stats();
        assert_eq!(snapshot.messages_sent, 0);
        assert_eq!(snapshot.bytes_sent, 0);
    }
}
