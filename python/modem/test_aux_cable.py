#!/usr/bin/env python3
"""Test aux-cable profile over a wired connection.

Physical setup:
- Laptop line-out → aux cable → RISC-V board line-in (or loopback on same machine)
- Or: laptop line-out → aux cable → laptop line-in (loopback test)

Usage:
    python3 test_aux_cable.py

Expected: Message received, test passes.
"""
import subprocess
import sys
import time

def test_loopback():
    """Send a message from TX to RX over aux cable."""
    test_msg = "AUX-CABLE-TEST-12345"
    
    # Start RX in background
    rx_proc = subprocess.Popen(
        [sys.executable, "main.py", "rx", "--profile", "aux-cable"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )
    
    time.sleep(1)  # Let RX initialize
    
    # Send TX
    tx_result = subprocess.run(
        [sys.executable, "main.py", "tx", "--profile", "aux-cable", test_msg],
        capture_output=True,
        text=True
    )
    
    print(f"TX: {tx_result.stdout}")
    
    # Wait for RX to receive
    time.sleep(2)
    rx_proc.terminate()
    rx_stdout, rx_stderr = rx_proc.communicate()
    
    print(f"RX: {rx_stdout.decode()}")
    
    # Verify
    if test_msg in rx_stdout.decode():
        print("✓ Aux cable loopback successful")
        return True
    else:
        print("✗ Aux cable loopback failed")
        return False

if __name__ == "__main__":
    success = test_loopback()
    sys.exit(0 if success else 1)
