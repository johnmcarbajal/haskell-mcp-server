# Corner Cases and Test Coverage Recommendations

## Executive Summary

Current test coverage: **~40%**  
Target coverage: **85%+**  
Critical gaps identified: **15 major areas**

## Critical Corner Cases Identified

### 1. Protocol Compliance Issues

**JSON-RPC Specification Violations:**
- Missing `jsonrpc` field in requests
- Invalid `jsonrpc` version (not "2.0")
- Wrong data types for required fields
- Malformed JSON handling
- Batch request processing (currently unsupported)

**Impact:** High - Could break compatibility with MCP clients

### 2. MCPVersion Edge Cases

**Problematic Inputs:**
```haskell
MCPVersion (-1) 5      -- Negative major version
MCPVersion 2024 (-1)   -- Negative minor version
MCPVersion 0 0         -- Zero version
MCPVersion 9999 99     -- Future versions
```

**String Format Issues:**
```
"invalid-format"       -- Missing components
"2024-13-01"          -- Invalid month
"2024-1-01"           -- Missing zero padding
```

**Impact:** Medium - Version negotiation failures

### 3. Text and Unicode Handling

**Dangerous Inputs:**
```
""                     -- Empty strings
"🎉💀🌍"               -- Emoji sequences
"Very long text..."    -- >1MB strings
"\0\x1F\x7F"          -- Control characters
"Text with\nnewlines"  -- Multiline content
```

**Impact:** Medium - Encoding/display issues

### 4. Calculator Expression Vulnerabilities

**Edge Cases:**
```
"1 / 0"               -- Division by zero
""                    -- Empty expression  
"1 + + 2"            -- Invalid syntax
"999999999999 * 999999999999"  -- Overflow
"1.7976931348623157e+308"      -- Double limits
"1 2 3"              -- Missing operator
"+ 1 2"              -- Prefix notation
```

**Impact:** High - Runtime exceptions

### 5. Base64 Processing Issues

**Invalid Inputs:**
```
"Invalid=Base64!"     -- Invalid characters
"SGVsbG8"            -- Missing padding
"SGVsbG8====="       -- Excessive padding
""                   -- Empty string
```

**Large Data:**
```
"Very large base64 encoded data..." -- Memory exhaustion
```

**Impact:** Medium - Data corruption/crashes

### 6. Resource Management Vulnerabilities

**Concurrency Issues:**
- Multiple clients accessing same resource
- Resource modification during read
- Resource cleanup during active requests
- STM transaction conflicts

**Resource URI Issues:**
```
""                   -- Empty URI
"invalid://format"   -- Malformed URI
"file:///etc/passwd" -- Path traversal attempts
```

**Impact:** High - Security/stability risks

### 7. Network and IO Edge Cases

**WebSocket Vulnerabilities:**
- Connection drops mid-message
- Very large messages (>1GB)
- Rapid connection/disconnection
- Invalid WebSocket frames
- Client timeout scenarios

**Stdio Mode Issues:**
- EOF on stdin
- Partial line reads
- Signal interruption (SIGINT, SIGTERM)
- Pipe broken scenarios

**Impact:** High - Service availability

### 8. Memory and Performance Issues

**Resource Exhaustion:**
- Too many concurrent connections
- Memory leaks from abandoned handlers
- Unbounded tool/resource lists
- Large message queues

**Performance Degradation:**
- O(n) tool lookups in large lists
- JSON parsing of huge objects
- Recursive tool calls
- Long-running tool handlers

**Impact:** High - DoS vulnerabilities

## Untested Tool Coverage Gaps

### Missing Tool Tests
1. **Random Number Generator**: Range validation, edge cases
2. **Text Analysis**: Unicode handling, empty text, large documents
3. **Weather Tool**: All location formats, mock data consistency
4. **UUID Generator**: Format validation, uniqueness
5. **Jeff's Predictor**: Date calculations, randomness bounds

### Tool Parameter Validation Gaps
```json
// Missing validations
{"min": "not_a_number"}        // Type errors
{"operation": "INVALID"}       // Case sensitivity  
{"text": null}                 // Null handling
{"arguments": [1,2,3]}         // Array vs object confusion
```

## High-Priority Test Additions

### 1. Protocol Compliance Suite (Critical)

```haskell
describe "JSON-RPC Protocol Compliance" $ do
  it "rejects missing jsonrpc field"
  it "validates jsonrpc version exactly"
  it "handles malformed JSON gracefully"
  it "preserves request ID in responses"
  it "returns proper error codes"
  it "handles null IDs correctly"
```

### 2. All Tool Coverage (High)

```haskell
describe "Complete Tool Validation" $ do
  it "validates random number ranges"
  it "handles text analysis edge cases"
  it "tests weather tool locations"
  it "verifies UUID format compliance"
  it "validates base64 edge cases"
  it "tests Jeff predictor date logic"
```

### 3. Error Handling Completeness (High)

```haskell
describe "Error Response Coverage" $ do
  it "returns correct error codes for each failure"
  it "includes helpful error messages"
  it "handles error data field properly"
  it "maintains error consistency"
```

### 4. Resource Management (Medium)

```haskell
describe "Resource Operations" $ do
  it "handles resource CRUD operations"
  it "validates resource URI formats" 
  it "manages concurrent resource access"
  it "cleans up resources properly"
```

### 5. Network Resilience (Medium)

```haskell
describe "Network Edge Cases" $ do
  it "handles connection drops gracefully"
  it "processes large messages correctly"
  it "manages concurrent connections"
  it "recovers from IO errors"
```

## Property-Based Testing Priorities

### 1. JSON Round-Trip Properties
- All type serialization/deserialization
- Invariant preservation
- Error handling consistency

### 2. Business Logic Properties  
- Tool result consistency
- Parameter validation rules
- State transition validity

### 3. Performance Properties
- Response time bounds
- Memory usage limits  
- Concurrency safety

## Integration Testing Needs

### 1. Full Protocol Flow Tests
- Complete initialize -> tools -> resources cycle
- Real WebSocket client simulation
- Stdio mode end-to-end testing

### 2. Load Testing
- Concurrent connection handling
- Message throughput testing
- Memory leak detection
- Performance regression testing

## Security Testing Requirements

### 1. Input Validation
- Injection attack prevention
- Buffer overflow protection
- Resource exhaustion limits

### 2. Access Control
- Resource permission validation
- Tool execution boundaries
- Client isolation

## Recommended Testing Tools

### 1. Property-Based Testing
- **QuickCheck**: For invariant testing
- **Hedgehog**: For stateful testing
- **validity**: For data validation

### 2. Integration Testing
- **WebSockets**: Real client simulation
- **Async**: Concurrency testing
- **Criterion**: Performance benchmarking

### 3. Security Testing
- **Input fuzzing**: Random input generation
- **Memory profiling**: Leak detection
- **Load testing**: Stress testing

## Implementation Roadmap

### Phase 1: Critical Gaps (Week 1)
1. JSON-RPC protocol compliance
2. All tool functionality coverage
3. Basic error handling

### Phase 2: Robustness (Week 2)  
1. Parameter validation edge cases
2. Resource management testing
3. Property-based test integration

### Phase 3: Production Readiness (Week 3)
1. Network resilience testing
2. Performance/load testing
3. Security vulnerability assessment

### Phase 4: Maintenance (Ongoing)
1. Regression test automation
2. Coverage monitoring
3. Performance benchmarking

## Success Metrics

- **Unit Test Coverage**: 85%+
- **Integration Test Coverage**: 70%+
- **Property Test Coverage**: 50%+
- **Zero Critical Vulnerabilities**
- **Response Time**: <100ms for 95th percentile
- **Memory Usage**: <100MB under normal load
- **Concurrent Connections**: 100+ supported

This comprehensive testing strategy will ensure production-ready reliability and security for the MCP server implementation.