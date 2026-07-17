#!/usr/bin/env python3
"""Test DCF-Control ops over aux cable.

Requires physical aux cable connection between TX and RX.
Sends a set_param control op and verifies transmission.
"""
import subprocess
import json
import sys

def test_control_op():
    """Send a set_param control op over aux cable."""
    control_op = {
        "op": "set_param",
        "slot": 0,
        "idx": 1,
        "value": 0.75,
        "id": "test-1"
    }
    
    # Serialize as DCF-Text message
    op_bytes = json.dumps(control_op).encode('utf-8')
    
    # Send via modem
    tx_result = subprocess.run(
        ["python3", "python/modem/main.py", "tx",
         "--profile", "aux-cable",
         op_bytes.hex()],
        capture_output=True,
        text=True
    )
    
    print(f"TX: {tx_result.stdout}")
    
    # Verify transmission succeeded
    if tx_result.returncode == 0:
        print("✓ Control op transmitted over aux cable")
        return True
    else:
        print("✗ Control op transmission failed")
        print(f"stderr: {tx_result.stderr}")
        return False

if __name__ == "__main__":
    success = test_control_op()
    sys.exit(0 if success else 1)
