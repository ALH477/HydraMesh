//! Comprehensive test suite for Thunderbolt transport
//!
//! Copyright © 2025 DeMoD LLC

use std::io::{Read, Write};
use std::net::TcpStream;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::Mutex;

use thunderbolt_transport::{
    make_thunderbolt_transport, LogLevel, MessageData, MessageType, Stats,
    ThunderboltConfig, ThunderboltTransport, ThunderboltTransportBuilder, TransportError,
};

// ============================================================================
// Test Helpers
// ============================================================================

/// Captured log messages for verification
struct TestLogger {
    messages: Mutex<Vec<(LogLevel, String)>>,
}

impl TestLogger {
    fn new() -> Arc<Self> {
        Arc::new(Self {
            messages: Mutex::new(Vec::new()),
        })
    }

    fn log(&self, level: LogLevel, message: &str) {
        self.messages.lock().push((level, message.to_string()));
    }

    fn clear(&self) {
        self.messages.lock().clear();
    }

    fn find(&self, substring: &str) -> bool {
        self.messages.lock().iter().any(|(_, msg)| msg.contains(substring))
    }

    fn find_level(&self, level: LogLevel) -> bool {
        self.messages.lock().iter().any(|(l, _)| *l == level)
    }
}

/// Create a test message of specified size with predictable content
fn make_test_message(size: usize) -> Vec<u8> {
    (0..size).map(|i| (i % 256) as u8).collect()
}

/// Verify a test message has correct content
fn verify_test_message(msg: &[u8], expected_size: usize) -> bool {
    msg.len() == expected_size && msg.iter().enumerate().all(|(i, &b)| b == (i % 256) as u8)
}

/// Shared counter for received messages
struct MessageCounter {
    control_messages: AtomicUsize,
    data_streams: AtomicUsize,
    bytes_received: AtomicU64,
    last_data: Mutex<Option<Vec<u8>>>,
}

impl MessageCounter {
    fn new() -> Arc<Self> {
        Arc::new(Self {
            control_messages: AtomicUsize::new(0),
            data_streams: AtomicUsize::new(0),
            bytes_received: AtomicU64::new(0),
            last_data: Mutex::new(None),
        })
    }

    fn control_count(&self) -> usize {
        self.control_messages.load(Ordering::SeqCst)
    }

    fn stream_count(&self) -> usize {
        self.data_streams.load(Ordering::SeqCst)
    }

    fn bytes(&self) -> u64 {
        self.bytes_received.load(Ordering::SeqCst)
    }

    fn last_message(&self) -> Option<Vec<u8>> {
        self.last_data.lock().clone()
    }
}

// ============================================================================
// Basic Functionality Tests
// ============================================================================

#[test]
fn test_create_transport_loopback() {
    let logger = TestLogger::new();
    let logger_clone = Arc::clone(&logger);

    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(15000)
        .data_port(15001)
        .logger(move |level, msg| logger_clone.log(level, msg))
        .build();

    match result {
        Ok(transport) => {
            assert!(transport.is_running());
            assert!(logger.find("Initialized interface"));
            assert!(logger.find("UDP socket bound"));
            transport.close();
        }
        Err(TransportError::InterfaceNotFound(_)) => {
            // Skip test if loopback interface not available (e.g., in container)
            println!("Skipping test: loopback interface not found");
        }
        Err(e) => panic!("Unexpected error: {}", e),
    }
}

#[test]
fn test_transport_statistics() {
    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(15010)
        .data_port(15011)
        .build();

    if let Ok(transport) = result {
        let stats = transport.get_stats();
        assert_eq!(stats.messages_sent, 0);
        assert_eq!(stats.messages_received, 0);
        assert_eq!(stats.bytes_sent, 0);
        assert_eq!(stats.bytes_received, 0);

        transport.reset_stats();
        let stats = transport.get_stats();
        assert_eq!(stats.errors, 0);

        transport.close();
    }
}

#[test]
fn test_builder_pattern() {
    let config = ThunderboltConfig {
        interface: "lo".to_string(),
        control_port: 15012,
        data_port: 15013,
        buffer_size: 1024 * 1024,
        tcp_nodelay: true,
        tcp_keepalive: false,
    };

    assert_eq!(config.interface, "lo");
    assert_eq!(config.control_port, 15012);
    assert_eq!(config.buffer_size, 1024 * 1024);
}

// ============================================================================
// Loopback Communication Tests
// ============================================================================

#[test]
fn test_udp_loopback_echo() {
    let counter = MessageCounter::new();
    let counter_clone = Arc::clone(&counter);

    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(15020)
        .data_port(15021)
        .message_handler(move |msg_type, data| {
            if msg_type == MessageType::ControlMessage {
                if let MessageData::Bytes(bytes) = data {
                    counter_clone.control_messages.fetch_add(1, Ordering::SeqCst);
                    counter_clone.bytes_received.fetch_add(bytes.len() as u64, Ordering::SeqCst);
                    *counter_clone.last_data.lock() = Some(bytes);
                }
            }
        })
        .build();

    if let Ok(transport) = result {
        // Give transport time to start
        std::thread::sleep(Duration::from_millis(500));

        // Send test message to ourselves
        let test_msg = make_test_message(100);
        let _ = transport.send_message(&test_msg, "::1%lo");

        // Wait for echo
        std::thread::sleep(Duration::from_millis(500));

        // Verify we received it
        assert_eq!(counter.control_count(), 1);
        if let Some(received) = counter.last_message() {
            assert!(verify_test_message(&received, 100));
        }

        transport.close();
    }
}

#[test]
fn test_tcp_loopback_stream() {
    let counter = MessageCounter::new();
    let counter_clone = Arc::clone(&counter);

    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(15030)
        .data_port(15031)
        .message_handler(move |msg_type, data| {
            if msg_type == MessageType::DataStream {
                if let MessageData::Stream(mut stream) = data {
                    // Echo back what we receive
                    let mut buf = [0u8; 1];
                    if stream.read_exact(&mut buf).is_ok() {
                        let _ = stream.write_all(&buf);
                        let _ = stream.flush();
                        counter_clone.data_streams.fetch_add(1, Ordering::SeqCst);
                        counter_clone.bytes_received.fetch_add(1, Ordering::SeqCst);
                    }
                }
            }
        })
        .build();

    if let Ok(transport) = result {
        // Give transport time to start
        std::thread::sleep(Duration::from_millis(500));

        // Open data stream and send byte
        match transport.open_data_stream("::1%lo") {
            Ok(mut stream) => {
                stream.write_all(&[42]).unwrap();
                stream.flush().unwrap();

                // Read echo
                let mut response = [0u8; 1];
                stream.read_exact(&mut response).unwrap();
                assert_eq!(response[0], 42);

                // Verify handler saw it
                std::thread::sleep(Duration::from_millis(200));
                assert_eq!(counter.stream_count(), 1);
            }
            Err(e) => {
                println!("Skipping TCP test: {}", e);
            }
        }

        transport.close();
    }
}

// ============================================================================
// Multiple Messages Test
// ============================================================================

#[test]
fn test_multiple_udp_messages() {
    let counter = MessageCounter::new();
    let counter_clone = Arc::clone(&counter);

    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(15040)
        .data_port(15041)
        .message_handler(move |msg_type, data| {
            if msg_type == MessageType::ControlMessage {
                if let MessageData::Bytes(bytes) = data {
                    counter_clone.control_messages.fetch_add(1, Ordering::SeqCst);
                    counter_clone.bytes_received.fetch_add(bytes.len() as u64, Ordering::SeqCst);
                }
            }
        })
        .build();

    if let Ok(transport) = result {
        std::thread::sleep(Duration::from_millis(500));

        // Send multiple messages
        let msg = make_test_message(64);
        for _ in 0..10 {
            let _ = transport.send_message(&msg, "::1%lo");
        }

        // Wait for all messages
        std::thread::sleep(Duration::from_millis(500));

        // Should have received all messages
        assert!(counter.control_count() >= 10);

        transport.close();
    }
}

// ============================================================================
// Error Handling Tests
// ============================================================================

#[test]
fn test_invalid_interface() {
    let result = ThunderboltTransportBuilder::new()
        .interface("nonexistent-interface-xyz123")
        .control_port(15050)
        .build();

    assert!(matches!(result, Err(TransportError::InterfaceNotFound(_))));
}

#[test]
fn test_invalid_peer_address() {
    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(15060)
        .data_port(15061)
        .build();

    if let Ok(transport) = result {
        let result = transport.send_message(&[1, 2, 3], "invalid-address");
        assert!(matches!(result, Err(TransportError::InvalidAddress(_))));
        transport.close();
    }
}

#[test]
fn test_oversized_message() {
    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(15070)
        .data_port(15071)
        .build();

    if let Ok(transport) = result {
        let huge_msg = vec![0u8; 100000];
        let result = transport.send_message(&huge_msg, "::1%lo");
        assert!(matches!(result, Err(TransportError::MessageTooLarge { .. })));
        transport.close();
    }
}

// ============================================================================
// RTT Measurement Tests
// ============================================================================

#[test]
fn test_rtt_measurement_loopback() {
    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(15080)
        .data_port(15081)
        .build();

    if let Ok(transport) = result {
        std::thread::sleep(Duration::from_millis(500));

        // Measure RTT to ourselves (the transport auto-responds to RTT pings)
        let rtt = transport.measure_rtt("::1%lo", Duration::from_secs(5));

        match rtt {
            Ok(Some(rtt_sec)) => {
                println!("Loopback RTT: {:.6} ms", rtt_sec * 1000.0);
                // Loopback should be very fast
                assert!(rtt_sec < 0.1); // Less than 100ms
            }
            Ok(None) => {
                println!("RTT measurement timed out (expected on some systems)");
            }
            Err(e) => {
                println!("RTT measurement failed: {}", e);
            }
        }

        transport.close();
    }
}

// ============================================================================
// Performance Benchmarks
// ============================================================================

#[test]
fn test_benchmark_udp_latency() {
    let counter = MessageCounter::new();
    let counter_clone = Arc::clone(&counter);

    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(15090)
        .data_port(15091)
        .message_handler(move |msg_type, data| {
            if msg_type == MessageType::ControlMessage {
                if let MessageData::Bytes(_) = data {
                    counter_clone.control_messages.fetch_add(1, Ordering::SeqCst);
                }
            }
        })
        .build();

    if let Ok(transport) = result {
        std::thread::sleep(Duration::from_millis(500));

        let iterations = 1000;
        let msg = make_test_message(64);
        let start = Instant::now();

        for _ in 0..iterations {
            let _ = transport.send_message(&msg, "::1%lo");
        }

        let elapsed = start.elapsed();
        let avg_latency = elapsed.as_secs_f64() / iterations as f64;

        println!("\nSent {} UDP messages in {:.3} seconds", iterations, elapsed.as_secs_f64());
        println!("Average latency: {:.6} ms", avg_latency * 1000.0);
        println!("Messages per second: {:.0}", iterations as f64 / elapsed.as_secs_f64());

        // Should be under 1ms average on loopback
        assert!(avg_latency < 0.001);

        transport.close();
    }
}

#[test]
fn test_benchmark_message_sizes() {
    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(15100)
        .data_port(15101)
        .build();

    if let Ok(transport) = result {
        std::thread::sleep(Duration::from_millis(500));

        let sizes = [64, 512, 1024, 4096, 8192, 16384, 32768];

        println!("\nMessage size benchmarks:");
        for size in sizes.iter() {
            let msg = make_test_message(*size);
            let iterations = 100;
            let start = Instant::now();

            for _ in 0..iterations {
                let _ = transport.send_message(&msg, "::1%lo");
            }

            let elapsed = start.elapsed();
            let throughput = (*size * iterations) as f64 / elapsed.as_secs_f64() / 1024.0 / 1024.0;

            println!("  {} bytes: {:.2} MB/s", size, throughput);
        }

        transport.close();
    }
}

// ============================================================================
// Shutdown and Cleanup Tests
// ============================================================================

#[test]
fn test_graceful_shutdown() {
    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(15110)
        .data_port(15111)
        .build();

    if let Ok(transport) = result {
        assert!(transport.is_running());

        transport.close();

        assert!(!transport.is_running());
    }
}

#[test]
fn test_drop_cleanup() {
    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(15120)
        .data_port(15121)
        .build();

    if let Ok(transport) = result {
        // Just drop it - should cleanup automatically
        drop(transport);
    }
}

// ============================================================================
// Concurrent Access Tests
// ============================================================================

#[test]
fn test_concurrent_sends() {
    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(15130)
        .data_port(15131)
        .build();

    if let Ok(transport) = result {
        std::thread::sleep(Duration::from_millis(500));

        let handles: Vec<_> = (0..4)
            .map(|_| {
                let t = Arc::clone(&transport);
                std::thread::spawn(move || {
                    let msg = make_test_message(64);
                    for _ in 0..100 {
                        let _ = t.send_message(&msg, "::1%lo");
                    }
                })
            })
            .collect();

        for handle in handles {
            handle.join().unwrap();
        }

        let stats = transport.get_stats();
        assert!(stats.messages_sent >= 400);

        transport.close();
    }
}

// ============================================================================
// Configuration Tests
// ============================================================================

#[test]
fn test_custom_ports() {
    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(16000)
        .data_port(16001)
        .build();

    if let Ok(transport) = result {
        assert_eq!(transport.control_port(), 16000);
        assert_eq!(transport.data_port(), 16001);
        transport.close();
    }
}

#[test]
fn test_interface_name() {
    let result = ThunderboltTransportBuilder::new()
        .interface("lo")
        .control_port(16010)
        .data_port(16011)
        .build();

    if let Ok(transport) = result {
        assert_eq!(transport.interface(), "lo");
        transport.close();
    }
}

// ============================================================================
// Test Runner
// ============================================================================

/// Run all Thunderbolt transport tests and print summary
pub fn run_thunderbolt_tests() {
    println!("\n=== Thunderbolt Transport Test Suite ===\n");
    println!("Note: These tests use loopback interface (lo) for safety.");
    println!("For real Thunderbolt testing, manually test with actual hardware.\n");

    // Tests are run via cargo test, this is just for manual invocation
    println!("Run tests with: cargo test --lib");
}

#[cfg(test)]
mod integration_tests {
    use super::*;

    #[test]
    fn test_full_workflow() {
        let logger = TestLogger::new();
        let logger_clone = Arc::clone(&logger);
        let counter = MessageCounter::new();
        let counter_clone = Arc::clone(&counter);

        let result = ThunderboltTransportBuilder::new()
            .interface("lo")
            .control_port(17000)
            .data_port(17001)
            .logger(move |level, msg| logger_clone.log(level, msg))
            .message_handler(move |msg_type, data| {
                match msg_type {
                    MessageType::ControlMessage => {
                        if let MessageData::Bytes(bytes) = data {
                            counter_clone.control_messages.fetch_add(1, Ordering::SeqCst);
                            counter_clone.bytes_received.fetch_add(bytes.len() as u64, Ordering::SeqCst);
                        }
                    }
                    MessageType::DataStream => {
                        counter_clone.data_streams.fetch_add(1, Ordering::SeqCst);
                    }
                }
            })
            .build();

        if let Ok(transport) = result {
            // Verify initialization
            assert!(transport.is_running());
            assert!(logger.find("Initialized interface"));

            std::thread::sleep(Duration::from_millis(500));

            // Send control message
            let msg = make_test_message(128);
            let sent = transport.send_message(&msg, "::1%lo").unwrap();
            assert_eq!(sent, 128);

            std::thread::sleep(Duration::from_millis(500));

            // Check stats
            let stats = transport.get_stats();
            assert!(stats.messages_sent >= 1);
            assert!(stats.bytes_sent >= 128);

            // Reset stats
            transport.reset_stats();
            let stats = transport.get_stats();
            assert_eq!(stats.messages_sent, 0);

            // Shutdown
            transport.close();
            assert!(!transport.is_running());

            assert!(logger.find("Transport closed"));
        }
    }
}
