# Haskell MCP Server

A Model Context Protocol (MCP) server implementation in Haskell using Stack.

## Features

- **JSON-RPC 2.0 Protocol**: Full compliance with MCP specification
- **WebSocket Communication**: Real-time bidirectional communication
- **Tool System**: Extensible tool registration and execution
- **Resource Management**: Resource listing and reading capabilities
- **Type Safety**: Leverages Haskell's type system for robust protocol handling

## Built-in Tools

1. **echo** - Echo back input messages
2. **current_time** - Get the current system time
3. **calculate** - Perform basic arithmetic calculations

## Built-in Resources

- **config://server.json** - Server configuration information

## Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) (Haskell build tool)
- GHC 9.2+ (will be installed automatically by Stack)

## Quick Start

1. **Clone and build the project:**
   ```bash
   git clone <your-repo-url>
   cd haskell-mcp-server
   stack build
   ```

2. **Run the server:**
   ```bash
   stack exec haskell-mcp-server-exe
   ```

   Or specify a custom port:
   ```bash
   stack exec haskell-mcp-server-exe -- 8080
   ```

3. **Test the server:**
   The server will listen on `ws://127.0.0.1:3000` (or your specified port) for WebSocket connections.

## Development

### Project Structure

```
haskell-mcp-server/
├── app/
│   └── Main.hs              # Application entry point
├── src/
│   └── MCP/
│       ├── Types.hs         # MCP protocol types
│       ├── Server.hs        # Core server implementation
│       └── Tools.hs         # Tool definitions and handlers
├── test/
│   └── Spec.hs             # Test suite
├── package.yaml            # Package configuration
├── stack.yaml              # Stack configuration
└── README.md
```

### Adding Custom Tools

To add a new tool, create a `Tool` definition and handler:

```haskell
myTool :: Tool
myTool = Tool
  { toolName = "my_tool"
  , description = "Description of what the tool does"
  , inputSchema = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "param" .= object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Parameter description" :: Text)
              ]
          ]
      , "required" .= (["param"] :: [Text])
      ]
  }

myToolHandler :: Value -> IO ToolResult
myToolHandler args = do
  -- Your tool logic here
  return $ ToolResult
    [ ContentItem "text" "Tool result" ]
    Nothing
```

Then register it in `Main.hs`:

```haskell
addTool server myTool myToolHandler
```

### Adding Custom Resources

Similarly, for resources:

```haskell
let myResource = Resource
      { resourceUri = "custom://my-resource"
      , resourceName = "My Resource"
      , resourceDescription = Just "Custom resource description"
      , mimeType = Just "application/json"
      }

addResource server myResource $ return $ object
  [ "data" .= ("resource content" :: Text) ]
```

## Testing

Run the test suite:

```bash
stack test
```

## Building for Production

Create an optimized build:

```bash
stack build --ghc-options="-O2"
```

## Protocol Compliance

This server implements the Model Context Protocol specification:

- **Initialization**: Proper handshake with protocol version negotiation
- **Tool Listing**: Dynamic tool discovery via `tools/list`
- **Tool Execution**: Safe tool execution via `tools/call`
- **Resource Management**: Resource listing and reading via `resources/list` and `resources/read`
- **Error Handling**: Standard JSON-RPC error responses

## Dependencies

Key dependencies include:

- `aeson` - JSON serialization/deserialization
- `websockets` - WebSocket server implementation
- `stm` - Software Transactional Memory for concurrent state
- `text` - Efficient text processing
- `containers` - Data structures (Map, etc.)

## License

BSD3 License - see LICENSE file for details.

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass (`stack test`)
6. Commit your changes (`git commit -am 'Add amazing feature'`)
7. Push to the branch (`git push origin feature/amazing-feature`)
8. Open a Pull Request

## Example Client Usage

Here's how to connect to the server using a WebSocket client:

### JavaScript Client Example

```javascript
const ws = new WebSocket('ws://127.0.0.1:3000');

// Initialize the connection
ws.onopen = () => {
  // Send initialize request
  ws.send(JSON.stringify({
    jsonrpc: "2.0",
    method: "initialize",
    params: {
      protocolVersion: { major: 2024, minor: 11 },
      capabilities: {
        roots: { listChanged: true },
        sampling: {}
      },
      clientInfo: {
        name: "test-client",
        version: "1.0.0"
      }
    },
    id: 1
  }));
};

ws.onmessage = (event) => {
  const response = JSON.parse(event.data);
  console.log('Received:', response);

  // After initialization, send initialized notification
  if (response.id === 1) {
    ws.send(JSON.stringify({
      jsonrpc: "2.0",
      method: "initialized",
      params: {}
    }));

    // List available tools
    ws.send(JSON.stringify({
      jsonrpc: "2.0",
      method: "tools/list",
      params: {},
      id: 2
    }));

    // Call echo tool
    ws.send(JSON.stringify({
      jsonrpc: "2.0",
      method: "tools/call",
      params: {
        name: "echo",
        arguments: { message: "Hello, MCP!" }
      },
      id: 3
    }));
  }
};
```

### Python Client Example

```python
import asyncio
import websockets
import json

async def test_client():
    uri = "ws://127.0.0.1:3000"

    async with websockets.connect(uri) as websocket:
        # Initialize
        init_request = {
            "jsonrpc": "2.0",
            "method": "initialize",
            "params": {
                "protocolVersion": {"major": 2024, "minor": 11},
                "capabilities": {
                    "roots": {"listChanged": True},
                    "sampling": {}
                },
                "clientInfo": {
                    "name": "python-test-client",
                    "version": "1.0.0"
                }
            },
            "id": 1
        }

        await websocket.send(json.dumps(init_request))
        response = await websocket.recv()
        print("Init response:", json.loads(response))

        # Send initialized notification
        await websocket.send(json.dumps({
            "jsonrpc": "2.0",
            "method": "initialized",
            "params": {}
        }))

        # List tools
        await websocket.send(json.dumps({
            "jsonrpc": "2.0",
            "method": "tools/list",
            "params": {},
            "id": 2
        }))

        tools_response = await websocket.recv()
        print("Tools:", json.loads(tools_response))

        # Call calculator tool
        await websocket.send(json.dumps({
            "jsonrpc": "2.0",
            "method": "tools/call",
            "params": {
                "name": "calculate",
                "arguments": {"expression": "2 + 3 * 4"}
            },
            "id": 3
        }))

        calc_response = await websocket.recv()
        print("Calculation result:", json.loads(calc_response))

# Run the client
asyncio.run(test_client())
```

## Configuration

The server can be configured by modifying the `defaultServer` function in `MCP/Server.hs`:

```haskell
-- Custom server configuration
customServer :: IO MCPServer
customServer = do
  server <- defaultServer
  return server
    { serverInfo = ServerInfo
        { name = "my-custom-server"
        , version = "1.0.0"
        , protocolVersion = MCPVersion 2024 11
        }
    }
```

## Troubleshooting

### Common Issues

1. **Port already in use**: Change the port number when starting the server
   ```bash
   stack exec haskell-mcp-server-exe -- 8080
   ```

2. **Build errors**: Ensure you have the latest Stack version
   ```bash
   stack upgrade
   stack clean
   stack build
   ```

3. **WebSocket connection refused**: Check that the server is running and the port is correct

### Debug Mode

For verbose logging, you can modify the server to include more debug output:

```haskell
-- In MCP/Server.hs, add more putStrLn statements
handleMessage server req = do
  putStrLn $ "Received method: " ++ T.unpack (method req)
  -- ... rest of the function
```

## Performance Notes

- The server uses STM (Software Transactional Memory) for thread-safe state management
- WebSocket connections are handled concurrently using lightweight Haskell threads
- Tool handlers should be designed to be non-blocking for optimal performance

## API Reference

### Core Methods

| Method | Description | Parameters |
|--------|-------------|------------|
| `initialize` | Initialize the MCP connection | `InitializeParams` |
| `initialized` | Notification that initialization is complete | None |
| `tools/list` | List available tools | None |
| `tools/call` | Execute a tool | `ToolCallArgs` |
| `resources/list` | List available resources | None |
| `resources/read` | Read a resource | `{uri: string}` |

### Error Codes

| Code | Description |
|------|-------------|
| -32700 | Parse error |
| -32600 | Invalid request |
| -32601 | Method not found |
| -32602 | Invalid params |
| -32603 | Internal error |

## Roadmap

- [ ] Add support for streaming tool responses
- [ ] Implement resource subscriptions
- [ ] Add built-in file system tools
- [ ] Implement tool result caching
- [ ] Add configuration file support
- [ ] Performance optimizations
- [ ] Docker containerization

## Support

For questions, issues, or contributions:

- Open an issue on GitHub
- Check the [MCP specification](https://spec.modelcontextprotocol.io/) for protocol details
- Review the Haskell documentation for language-specific questions
