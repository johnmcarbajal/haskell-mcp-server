#!/usr/bin/env python3
"""
Debug what JSON your Haskell server is actually sending
"""
import subprocess
import json
import sys

def test_server_json():
    print("🔍 Testing Haskell server JSON output...")

    # Test different requests
    test_requests = [
        {
            "name": "initialize",
            "request": {
                "jsonrpc": "2.0",
                "method": "initialize",
                "params": {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {},
                    "clientInfo": {"name": "test", "version": "1.0"}
                },
                "id": 1
            }
        },
        {
            "name": "tools/list",
            "request": {
                "jsonrpc": "2.0",
                "method": "tools/list",
                "params": {},
                "id": 2
            }
        }
    ]

    for test in test_requests:
        print(f"\n📤 Testing {test['name']} request...")

        # Start server
        proc = subprocess.Popen(
            ["stack", "exec", "haskell-mcp-server-exe", "--", "--stdio"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            cwd="/Users/johncarbajal/src/haskell-mcp-server"
        )

        # Send request
        request_json = json.dumps(test['request']) + "\n"
        proc.stdin.write(request_json)
        proc.stdin.flush()
        proc.stdin.close()

        # Read all output lines
        stdout_lines = []
        while True:
            line = proc.stdout.readline()
            if not line:
                break
            line = line.strip()
            if line:
                stdout_lines.append(line)

        proc.wait()

        print(f"📥 Raw stdout lines ({len(stdout_lines)}):")
        for i, line in enumerate(stdout_lines):
            print(f"  Line {i+1}: {repr(line)}")

            # Try to parse each line as JSON
            try:
                parsed = json.loads(line)
                print(f"    ✅ Valid JSON: {json.dumps(parsed, indent=2)}")

                # Check required fields
                print(f"    📋 JSON-RPC Analysis:")
                print(f"       jsonrpc: {parsed.get('jsonrpc', 'MISSING')}")
                print(f"       id: {parsed.get('id', 'MISSING')} (type: {type(parsed.get('id'))})")
                print(f"       method: {parsed.get('method', 'MISSING')}")
                print(f"       result: {'present' if 'result' in parsed else 'MISSING'}")
                print(f"       error: {'present' if 'error' in parsed else 'MISSING'}")

            except json.JSONDecodeError as e:
                print(f"    ❌ Invalid JSON: {e}")
                print(f"    Raw: {repr(line)}")

if __name__ == "__main__":
    test_server_json()
