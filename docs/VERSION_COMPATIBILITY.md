# MCP Version Compatibility Guide

## Overview

The MCP (Model Context Protocol) uses semantic versioning for protocol compatibility. This document describes the version compatibility rules and provides guidance for implementing version negotiation between MCP clients and servers.

## Version Format

MCP versions follow the format: `MAJOR-MINOR-01`

- **Major version**: Indicates breaking changes in the protocol
- **Minor version**: Indicates backward-compatible additions
- **Day component**: Always "01" (ignored in compatibility checks)

Example: `2024-11-01` represents major version 2024, minor version 11.

## Compatibility Rules

### 1. Major Version Compatibility

- **Same major version = Compatible**
- **Different major version = Incompatible**

This is the fundamental rule: clients and servers with different major versions cannot communicate.

### 2. Minor Version Compatibility

Within the same major version:

- **Server perspective**: Can handle clients with same or older minor versions
- **Client perspective**: Can work with servers with same or newer minor versions

This ensures backward compatibility: newer servers support older clients, and older clients can connect to newer servers (though they may not use new features).

## Version Comparison Functions

### `compareVersions`

```haskell
compareVersions :: MCPVersion -> MCPVersion -> Ordering
```

Compares two versions, returning:
- `LT` if the first version is older
- `GT` if the first version is newer  
- `EQ` if versions are identical

### `isCompatible`

```haskell
isCompatible :: MCPVersion -> MCPVersion -> Bool
```

Returns `True` if two versions have the same major version (and thus can communicate).

### `canServerHandleClient`

```haskell
canServerHandleClient :: MCPVersion -> MCPVersion -> Bool
```

Checks if a server can handle a specific client version. Returns `True` if:
1. Major versions match
2. Server's minor version ≥ client's minor version

### `canClientWorkWithServer`

```haskell
canClientWorkWithServer :: MCPVersion -> MCPVersion -> Bool
```

Checks if a client can work with a specific server version. Returns `True` if:
1. Major versions match
2. Server's minor version ≥ client's minor version

## Version Negotiation Scenarios

### Successful Negotiation

```
Server: 2024-11
Client: 2024-10
Result: ✓ Compatible (server supports older client)

Server: 2024-10  
Client: 2024-11
Result: ✓ Compatible (client can work with older server)
```

### Failed Negotiation

```
Server: 2024-11
Client: 2025-11  
Result: ✗ Incompatible (major version mismatch)

Server: 2024-10
Client: 2024-11
Result: ✗ Server cannot handle newer client features
```

## Implementation Example

```haskell
-- During client connection
handleClientConnection :: MCPVersion -> MCPVersion -> IO (Maybe Connection)
handleClientConnection serverVersion clientVersion = 
    if canServerHandleClient serverVersion clientVersion
    then do
        logInfo $ "Accepted client version: " ++ show clientVersion
        establishConnection
    else do
        logWarning $ "Rejected incompatible client version: " ++ show clientVersion
        return Nothing

-- Finding best version from client preferences
negotiateVersion :: MCPVersion -> [MCPVersion] -> Maybe MCPVersion
negotiateVersion serverVersion clientPreferences =
    case filter (canServerHandleClient serverVersion) clientPreferences of
        [] -> Nothing
        compatible -> Just (head compatible)  -- First compatible is best
```

## Best Practices

1. **Always check compatibility** before establishing a connection
2. **Log version mismatches** for debugging
3. **Provide clear error messages** when rejecting connections
4. **Support version negotiation** where clients can propose multiple versions
5. **Document version requirements** in your API documentation

## Migration Guidelines

When upgrading versions:

### Minor Version Upgrade (e.g., 2024-11 → 2024-12)

- ✓ Existing clients continue to work
- ✓ New features available to updated clients
- ✓ No breaking changes allowed

### Major Version Upgrade (e.g., 2024-11 → 2025-01)

- ⚠️ Breaking change - incompatible with previous versions
- ⚠️ All clients must upgrade
- ⚠️ Consider running multiple server versions during transition

## Testing Version Compatibility

Use the provided test suite to verify compatibility logic:

```bash
stack test --match "Version Compatibility"
```

Key test scenarios covered:
- Version comparison ordering
- Compatibility checking
- Server-client negotiation
- Edge cases (version 0-0, overflow handling)
- Multiple version preference handling