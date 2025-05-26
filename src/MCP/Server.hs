{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server (
    MCPServer (..),
    defaultServer,
    runMCPServer,
    runStdioServer,
    handleMessage,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception (IOException, catch, finally)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Network.WebSockets as WS
import System.IO (hFlush, hIsEOF, hPutStrLn, isEOF, stderr, stdin, stdout)

import MCP.Types

-- | MCP Server State - keeping this as is since it's well-structured
data MCPServer = MCPServer
    { serverInfo :: ServerInfo
    , tools :: TVar [Tool]
    , resources :: TVar [Resource]
    , toolHandlers :: TVar (Map.Map Text (Value -> IO ToolResult))
    , resourceHandlers :: TVar (Map.Map Text (IO Value))
    , initialized :: TVar Bool
    }

-- | Default server configuration - simplified using applicative style
defaultServer :: IO MCPServer
defaultServer =
    MCPServer
        <$> pure defaultServerInfo
        <*> newTVarIO []
        <*> newTVarIO []
        <*> newTVarIO Map.empty
        <*> newTVarIO Map.empty
        <*> newTVarIO False
  where
    defaultServerInfo =
        ServerInfo
            { serverName = "haskell-mcp-server"
            , serverVersion = "0.1.0"
            , serverProtocolVersion = MCPVersion 2024 11
            }

-- | Centralized logging helper to reduce repetition
logInfo :: String -> IO ()
logInfo msg = hPutStrLn stderr msg >> hFlush stderr

-- | Server startup message - extracted to reduce duplication
logServerStartup :: IO ()
logServerStartup = do
    logInfo "Available tools: echo, current_time, calculate, random_number, text_analysis, weather, generate_uuid, base64, jeff_invite_predictor"
    logInfo "Available resources: config://server.json"

-- | JSON-RPC response builders - eliminating repetitive response construction
mkSuccessResponse :: (ToJSON a) => Maybe Value -> a -> JSONRPCResponse
mkSuccessResponse reqId result =
    JSONRPCResponse
        { responseJsonrpc = "2.0"
        , responseResult = Just (toJSON result)
        , responseError = Nothing
        , responseId = reqId
        }

mkErrorResponse :: Maybe Value -> Int -> Text -> JSONRPCResponse
mkErrorResponse reqId code msg =
    JSONRPCResponse
        { responseJsonrpc = "2.0"
        , responseResult = Nothing
        , responseError = Just $ JSONRPCError code msg Nothing
        , responseId = reqId
        }

-- | Common error codes as constants for better maintainability
parseError, invalidRequest, methodNotFound, invalidParams :: Int
parseError = -32700
invalidRequest = -32600
methodNotFound = -32601
invalidParams = -32602

-- | WebSocket server runner - simplified error handling
runMCPServer :: MCPServer -> Int -> IO ()
runMCPServer server port = do
    logInfo $ "Starting MCP Server on port " ++ show port
    logServerStartup
    WS.runServer "127.0.0.1" port $ \pending -> do
        conn <- WS.acceptRequest pending
        logInfo "Client connected"
        WS.withPingThread conn 30 (return ()) $
            handleClient server conn `finally` logInfo "Client disconnected"

-- | Stdio server with cleaner message processing loop
runStdioServer :: MCPServer -> IO ()
runStdioServer server = do
    logInfo "Starting MCP Server in stdio mode"
    logServerStartup
    logInfo "Server ready, waiting for messages..."

    readMessageLoop `catch` \(e :: IOException) -> do
        logInfo $ "Connection ended: " ++ show e
        logInfo "Server shutting down"
  where
    readMessageLoop = forever $ do
        logInfo "Waiting for next message..."
        -- Simplified timeout handling with race
        race (threadDelay 5000000) getLine >>= \case
            Left _ -> logInfo "Timeout waiting for message - client may have disconnected"
            Right line -> processStdioMessage server line

-- | Process a single stdio message - extracted for clarity
processStdioMessage :: MCPServer -> String -> IO ()
processStdioMessage server line = do
    logInfo $ "Received message: " ++ take 100 line ++ "..."
    case eitherDecode (L8.pack line) of
        Left err -> logInfo $ "Parse error: " ++ err
        Right request -> do
            logInfo $ "Processing request: " ++ T.unpack (requestMethod request)
            handleMessage server request >>= \case
                Just resp -> do
                    let respStr = L8.unpack (encode resp)
                    logInfo $ "Sending response: " ++ take 100 respStr ++ "..."
                    L8.putStrLn (encode resp) >> hFlush stdout
                    logInfo $ "Response sent for: " ++ T.unpack (requestMethod request)
                Nothing -> logInfo $ "No response needed for: " ++ T.unpack (requestMethod request)

-- | WebSocket client handler - simplified with better error handling
handleClient :: MCPServer -> WS.Connection -> IO ()
handleClient server conn = forever $ do
    msg <- WS.receiveData conn
    case eitherDecode msg of
        Left err -> do
            logInfo $ "Failed to decode message: " ++ err
            WS.sendTextData conn . encode $ mkErrorResponse Nothing parseError "Parse error"
        Right request ->
            handleMessage server request >>= mapM_ (WS.sendTextData conn . encode)

-- | Main message dispatcher - using case expressions more idiomatically
handleMessage :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleMessage server req = case requestMethod req of
    "initialize" -> handleInitialize server req
    "initialized" -> handleInitialized server req
    "tools/list" -> handleListTools server req
    "tools/call" -> handleCallTool server req
    "resources/list" -> handleListResources server req
    "resources/read" -> handleReadResource server req
    _ -> return . Just $ mkErrorResponse (requestId req) methodNotFound "Method not found"

-- | Initialize handler - cleaner parameter parsing and response building
handleInitialize :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleInitialize server req = do
    logInfo "Handling initialize request"
    case requestParams req >>= parseInitParams of
        Nothing -> do
            logInfo "Failed to parse initialize params"
            return . Just $ mkErrorResponse (requestId req) invalidParams "Invalid params"
        Just initParams -> do
            logInfo $ "Client protocol version: " ++ show (initProtocolVersion initParams)
            logInfo $ "Client info: " ++ show (initClientInfo initParams)

            let response = buildInitializeResponse
            logInfo "Sending initialize response with protocol version: 2024-11-05"
            return . Just $ mkSuccessResponse (requestId req) response
  where
    parseInitParams p = case fromJSON p of
        Success params -> Just params
        Error _ -> Nothing

    buildInitializeResponse =
        object
            [ "protocolVersion" .= ("2024-11-05" :: Text)
            , "capabilities"
                .= object
                    [ "tools" .= object ["listChanged" .= True]
                    , "resources" .= object ["subscribe" .= False, "listChanged" .= True]
                    ]
            , "serverInfo"
                .= object
                    [ "name" .= ("haskell-mcp-server" :: Text)
                    , "version" .= ("0.1.0" :: Text)
                    ]
            ]

-- | Initialized notification handler - using atomically for STM operation
handleInitialized :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleInitialized server _ = do
    atomically $ writeTVar (initialized server) True
    logInfo "Server initialized"
    return Nothing

-- | Tools list handler - simplified with direct STM read
handleListTools :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleListTools server req = do
    toolsList <- readTVarIO (tools server)
    return . Just $ mkSuccessResponse (requestId req) (object ["tools" .= toolsList])

-- | Tool call handler - cleaner parameter parsing and error handling
handleCallTool :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleCallTool server req =
    case requestParams req >>= parseToolCall of
        Nothing ->
            return . Just $ mkErrorResponse (requestId req) invalidParams "Invalid params"
        Just (toolName, args) -> do
            handlers <- readTVarIO (toolHandlers server)
            case Map.lookup toolName handlers of
                Nothing ->
                    return . Just $ mkErrorResponse (requestId req) methodNotFound "Tool not found"
                Just handler -> do
                    result <- handler args
                    return . Just $ mkSuccessResponse (requestId req) result
  where
    parseToolCall p = case fromJSON p of
        Success (ToolCallArgs toolName args) -> Just (toolName, args)
        Error _ -> Nothing

-- | Resources list handler - following same pattern as tools
handleListResources :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleListResources server req = do
    resourcesList <- readTVarIO (resources server)
    return . Just $ mkSuccessResponse (requestId req) (object ["resources" .= resourcesList])

-- | Resource read handler - consistent with tool call pattern
handleReadResource :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleReadResource server req =
    case requestParams req >>= parseResourceUri of
        Nothing ->
            return . Just $ mkErrorResponse (requestId req) invalidParams "Invalid params"
        Just uri -> do
            handlers <- readTVarIO (resourceHandlers server)
            case Map.lookup uri handlers of
                Nothing ->
                    return . Just $ mkErrorResponse (requestId req) methodNotFound "Resource not found"
                Just handler -> do
                    content <- handler
                    return . Just $ mkSuccessResponse (requestId req) content
  where
    parseResourceUri p = case fromJSON p of
        Success uri -> Just uri
        Error _ -> Nothing
