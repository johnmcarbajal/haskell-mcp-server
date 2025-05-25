#!/usr/bin/env python3
"""
Test the full MCP protocol flow like Claude Desktop would
"""
import subprocess
import json
import time
import threading

def test_full_mcp_flow():
    print("🔍 Testing full MCP protocol flow...")

    # Start server
    proc = subprocess.Popen(
        ["stack", "exec", "haskell-mcp-server-exe", "--", "--stdio"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        cwd="/Users/johncarbajal/src/haskell-mcp-server"
    )

    def read_stderr():
        """Read stderr in background"""
        while proc.poll() is None:
            line = proc.stderr.readline()
            if line:
                print(f"🟡 STDERR: {line.strip()}")

    # Start stderr reader
    stderr_thread = threading.Thread(target=read_stderr, daemon=True)
    stderr_thread.start()

    try:
        # Step 1: Initialize
        print("\n📤 Step 1: Initialize")
        init_request = {
            "jsonrpc": "2.0",
            "method": "initialize",
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {
                    "roots": {"listChanged": True},
                    "sampling": {}
                },
                "clientInfo": {
                    "name": "claude-desktop",
                    "version": "0.7.0"
                }
            },
            "id": 1
        }

        send_request(proc, init_request)
        response1 = read_response(proc)
        print(f"📥 Initialize Response: {json.dumps(response1, indent=2)}")

        # Step 2: List Tools
        print("\n📤 Step 2: List Tools")
        tools_request = {
            "jsonrpc": "2.0",
            "method": "tools/list",
            "params": {},
            "id": 2
        }

        send_request(proc, tools_request)
        response2 = read_response(proc)
        print(f"📥 Tools Response: {json.dumps(response2, indent=2)}")

        # Step 3: Call a Tool
        print("\n📤 Step 3: Call echo tool")
        call_request = {
            "jsonrpc": "2.0",
            "method": "tools/call",
            "params": {
                "name": "echo",
                "arguments": {
                    "message": "Hello from test!"
                }
            },
            "id": 3
        }

        send_request(proc, call_request)
        response3 = read_response(proc)
        print(f"📥 Tool Call Response: {json.dumps(response3, indent=2)}")

        # Step 4: List Resources
        print("\n📤 Step 4: List Resources")
        resources_request = {
            "jsonrpc": "2.0",
            "method": "resources/list",
            "params": {},
            "id": 4
        }

        send_request(proc, resources_request)
        response4 = read_response(proc)
        print(f"📥 Resources Response: {json.dumps(response4, indent=2)}")

        print("\n✅ All requests completed successfully!")

    except Exception as e:
        print(f"❌ Error during test: {e}")
    finally:
        proc.terminate()
        proc.wait()

def send_request(proc, request):
    """Send a JSON-RPC request"""
    json_str = json.dumps(request) + "\n"
    print(f"   Sending: {json_str.strip()}")
    proc.stdin.write(json_str)
    proc.stdin.flush()

def read_response(proc, timeout=10):
    """Read a JSON-RPC response with timeout"""
    start_time = time.time()
    while time.time() - start_time < timeout:
        line = proc.stdout.readline()
        if line:
            line = line.strip()
            if line:
                try:
                    return json.loads(line)
                except json.JSONDecodeError as e:
                    print(f"   ❌ Invalid JSON: {e}")
                    print(f"   Raw line: {repr(line)}")
                    continue
        time.sleep(0.1)

    raise TimeoutError("No response received within timeout")

if __name__ == "__main__":
    test_full_mcp_flow()
