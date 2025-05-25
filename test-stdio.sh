#!/bin/bash

# Test script for stdio MCP server lifecycle
echo "Testing Haskell MCP Server stdio lifecycle..."

# Build the server first
echo "Building server..."
stack build

# Create a temporary file with test messages
cat > test_messages.txt << 'EOF'
{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":{"major":2024,"minor":11},"capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}},"id":0}
{"jsonrpc":"2.0","method":"initialized","params":{}}
{"jsonrpc":"2.0","method":"tools/list","params":{},"id":1}
{"jsonrpc":"2.0","method":"tools/call","params":{"name":"echo","arguments":{"message":"Hello from lifecycle test!"}},"id":2}
EOF

echo "Sending test messages..."
cat test_messages.txt | stack exec haskell-mcp-server-exe -- --stdio

# Clean up
rm test_messages.txt

echo "Test complete. The server should have processed all messages and then shut down gracefully."
