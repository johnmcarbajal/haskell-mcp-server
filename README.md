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
