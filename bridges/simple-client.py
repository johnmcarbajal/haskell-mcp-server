import subprocess
import json
import time

# Start your server
proc = subprocess.Popen(
    ["stack", "exec", "haskell-mcp-server-exe", "--", "--stdio"],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True
)

# Send initialize request
request = {
    "jsonrpc": "2.0",
    "method": "initialize",
    "params": {
        "protocolVersion": "2024-11-05",
        "capabilities": {},
        "clientInfo": {"name": "test", "version": "1.0"}
    },
    "id": 1
}

proc.stdin.write(json.dumps(request) + "\n")
proc.stdin.flush()

# Wait for response with longer timeout
time.sleep(2)  # Give it time
response = proc.stdout.readline()
print(f"Response: {response}")
