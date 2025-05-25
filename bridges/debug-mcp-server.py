#!/usr/bin/env python3
"""
Debug script to see what the Haskell MCP server actually outputs
"""
import asyncio
import subprocess
import json

async def debug_mcp_server():
    print("🔍 Debugging Haskell MCP server output...")

    try:
        # Start the MCP server
        process = await asyncio.create_subprocess_exec(
            "stack", "exec", "haskell-mcp-server-exe", "--", "--stdio",
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE
        )
        print("✅ MCP server process started")

        # Send initialize message
        init_message = {
            "jsonrpc": "2.0",
            "method": "initialize",
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {"name": "debug", "version": "1.0"}
            },
            "id": 1
        }

        message_str = json.dumps(init_message) + "\n"
        print(f"📤 Sending: {message_str.strip()}")

        # Send the message
        process.stdin.write(message_str.encode())
        await process.stdin.drain()

        # Wait for response with timeout
        try:
            # FIXED: Read stderr and stdout sequentially, don't cancel!

            # First, try to read stderr (server logs) with short timeout
            try:
                stderr_output = await asyncio.wait_for(process.stderr.read(2048), timeout=2)
                if stderr_output:
                    print(f"📋 Server logs (stderr):")
                    print(stderr_output.decode().strip())
            except asyncio.TimeoutError:
                print("📋 No stderr output received")

            # Then read stdout (JSON response) with longer timeout
            try:
                stdout_line = await asyncio.wait_for(process.stdout.readline(), timeout=15)
                if stdout_line:
                    response_str = stdout_line.decode().strip()
                    print(f"📥 Server response (stdout): {response_str}")
                    if response_str:
                        try:
                            response_json = json.loads(response_str)
                            print("✅ Valid JSON response received")
                            print(f"   Response ID: {response_json.get('id')}")
                            print(f"   Has result: {'result' in response_json}")
                            print(f"   Has error: {'error' in response_json}")
                        except json.JSONDecodeError as e:
                            print(f"❌ Invalid JSON: {e}")
                            print(f"   Raw response: '{response_str}'")
                    else:
                        print("❌ Empty response from server")
                else:
                    print("❌ No stdout response received")
            except asyncio.TimeoutError:
                print("❌ Timeout waiting for server response")

        except Exception as e:
            print(f"❌ Error during communication: {e}")

        # Try to get any remaining stderr output
        try:
            remaining_stderr = await asyncio.wait_for(process.stderr.read(1024), timeout=1)
            if remaining_stderr:
                print(f"📋 Additional server logs:")
                print(remaining_stderr.decode().strip())
        except asyncio.TimeoutError:
            pass

        # Clean up
        process.terminate()
        await process.wait()

    except FileNotFoundError:
        print("❌ Stack command not found")
        print("   Make sure you're in the haskell-mcp-server directory")
        print("   And that stack is installed")
    except Exception as e:
        print(f"❌ Error: {e}")

if __name__ == "__main__":
    asyncio.run(debug_mcp_server())
