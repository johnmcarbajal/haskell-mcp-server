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
import Control.Monad (forever, void)
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

-- | MCP Server State
data MCPServer = MCPServer
    { serverInfo :: ServerInfo
    , tools :: TVar [Tool]
    , resources :: TVar [Resource]
    , toolHandlers :: TVar (Map.Map Text (Value -> IO ToolResult))
    , resourceHandlers :: TVar (Map.Map Text (IO Value))
    , initialized :: TVar Bool
    }

-- | Default MCP Server configuration
defaultServer :: IO MCPServer
defaultServer = do
    toolsVar <- newTVarIO []
    resourcesVar <- newTVarIO []
    handlersVar <- newTVarIO Map.empty
    resourceHandlersVar <- newTVarIO Map.empty
    initializedVar <- newTVarIO False

    return
        MCPServer
            { serverInfo =
                ServerInfo
                    { serverName = "haskell-mcp-server"
                    , serverVersion = "0.1.0"
                    , serverProtocolVersion = MCPVersion 2024 11
                    }
            , tools = toolsVar
            , resources = resourcesVar
            , toolHandlers = handlersVar
            , resourceHandlers = resourceHandlersVar
            , initialized = initializedVar
            }

-- | Run the MCP Server via WebSocket
runMCPServer :: MCPServer -> Int -> IO ()
runMCPServer server port = do
    hPutStrLn stderr $ "Starting MCP Server on port " ++ show port
    hPutStrLn stderr "Available tools: echo, current_time, calculate, random_number, text_analysis, weather, generate_uuid, base64"
    hPutStrLn stderr "Available resources: config://server.json"
    WS.runServer "127.0.0.1" port $ \pending -> do
        conn <- WS.acceptRequest pending
        hPutStrLn stderr "Client connected"
        WS.withPingThread conn 30 (return ()) $ do
            handleClient server conn

-- | Run the MCP Server via stdio
runStdioServer :: MCPServer -> IO ()
runStdioServer server = do
    hPutStrLn stderr "Starting MCP Server in stdio mode"
    hPutStrLn stderr "Available tools: echo, current_time, calculate, random_number, text_analysis, weather, generate_uuid, base64"
    hPutStrLn stderr "Available resources: config://server.json"
    hPutStrLn stderr "Server ready, waiting for messages..."
    hFlush stderr

    -- Keep reading messages until EOF
    let readLoop = do
            hPutStrLn stderr "Waiting for next message..."
            hFlush stderr

            -- Try to read with a timeout to detect if client has closed connection
            result <- race (threadDelay 5000000) getLine -- 5 second timeout
            case result of
                Left _ -> do
                    hPutStrLn stderr "Timeout waiting for message - client may have disconnected"
                    hFlush stderr
                    readLoop -- Try again
                Right line -> do
                    hPutStrLn stderr $ "Received message: " ++ take 100 line ++ "..."
                    let lineBS = L8.pack line
                    case eitherDecode lineBS of
                        Left err -> do
                            hPutStrLn stderr $ "Parse error: " ++ err
                            hFlush stderr
                            readLoop -- Continue even if there's a parse error
                        Right request -> do
                            hPutStrLn stderr $ "Processing request: " ++ T.unpack (requestMethod request)
                            response <- handleMessage server request
                            case response of
                                Just resp -> do
                                    let respStr = L8.unpack (encode resp)
                                    hPutStrLn stderr $ "Sending response: " ++ take 100 respStr ++ "..."
                                    L8.putStrLn (encode resp)
                                    hFlush stdout
                                    hPutStrLn stderr $ "Response sent for: " ++ T.unpack (requestMethod request)
                                Nothing ->
                                    hPutStrLn stderr $ "No response needed for: " ++ T.unpack (requestMethod request)
                            hFlush stderr
                            readLoop -- Continue reading more messages

    -- Handle EOF gracefully
    catch readLoop $ \(e :: IOException) -> do
        hPutStrLn stderr $ "Connection ended: " ++ show e
        hPutStrLn stderr "Server shutting down"
        hFlush stderr

-- | Handle client connection
handleClient :: MCPServer -> WS.Connection -> IO ()
handleClient server conn = flip finally disconnect $ do
    hPutStrLn stderr "Handling client messages..."
    forever $ do
        msg <- WS.receiveData conn
        case eitherDecode msg of
            Left err -> do
                hPutStrLn stderr $ "Failed to decode message: " ++ err
                sendError conn Nothing (-32700) "Parse error"
            Right request -> do
                response <- handleMessage server request
                case response of
                    Just resp -> WS.sendTextData conn (encode resp)
                    Nothing -> return ()
  where
    disconnect = hPutStrLn stderr "Client disconnected"

-- | Send JSON-RPC error response
sendError :: WS.Connection -> Maybe Value -> Int -> Text -> IO ()
sendError conn reqId code msg = do
    let errorResp =
            JSONRPCResponse
                { responseJsonrpc = "2.0"
                , responseResult = Nothing
                , responseError = Just $ JSONRPCError code msg Nothing
                , responseId = reqId
                }
    WS.sendTextData conn (encode errorResp)

-- | Handle incoming JSON-RPC messages
handleMessage :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleMessage server req = case requestMethod req of
    "initialize" -> handleInitialize server req
    "initialized" -> handleInitialized server req
    "tools/list" -> handleListTools server req
    "tools/call" -> handleCallTool server req
    "resources/list" -> handleListResources server req
    "resources/read" -> handleReadResource server req
    _ ->
        return $
            Just $
                JSONRPCResponse
                    { responseJsonrpc = "2.0"
                    , responseResult = Nothing
                    , responseError = Just $ JSONRPCError (-32601) "Method not found" Nothing
                    , responseId = requestId req
                    }

-- | Handle initialize request
handleInitialize :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleInitialize server req = do
    hPutStrLn stderr "Handling initialize request"
    case requestParams req of
        Nothing -> do
            hPutStrLn stderr "No params in initialize request"
            return $ Just $ errorResponse (-32602) "Invalid params"
        Just p -> case fromJSON p of
            Error err -> do
                hPutStrLn stderr $ "Failed to parse initialize params: " ++ err
                return $ Just $ errorResponse (-32602) (T.pack err)
            Success initParams -> do
                hPutStrLn stderr $ "Client protocol version: " ++ show (initProtocolVersion initParams)
                hPutStrLn stderr $ "Client info: " ++ show (initClientInfo initParams)

                -- Echo back the protocol version as a string (the format Claude Desktop expects)
                let protocolVersionStr = "2024-11-05" -- Match what Claude Desktop sent
                let result =
                        object
                            [ "protocolVersion" .= protocolVersionStr
                            , "capabilities"
                                .= object
                                    [ "tools" .= object ["listChanged" .= True]
                                    , "resources" .= object ["subscribe" .= False, "listChanged" .= True]
                                    ]
                            , "serverInfo"
                                .= object
                                    [ "name" .= ("haskell-mcp-server" :: T.Text)
                                    , "version" .= ("0.1.0" :: T.Text)
                                    ]
                            ]

                hPutStrLn stderr $ "Sending initialize response with protocol version: " ++ protocolVersionStr
                return $
                    Just $
                        JSONRPCResponse
                            { responseJsonrpc = "2.0"
                            , responseResult = Just result
                            , responseError = Nothing
                            , responseId = requestId req
                            }
  where
    errorResponse code msg =
        JSONRPCResponse
            { responseJsonrpc = "2.0"
            , responseResult = Nothing
            , responseError = Just $ JSONRPCError code msg Nothing
            , responseId = requestId req
            }

-- | Handle initialized notification
handleInitialized :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleInitialized server _ = do
    atomically $ writeTVar (initialized server) True
    hPutStrLn stderr "Server initialized"
    return Nothing

-- | Handle tools/list request
handleListTools :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleListTools server req = do
    toolsList <- readTVarIO (tools server)
    let result = object ["tools" .= toolsList]

    return $
        Just $
            JSONRPCResponse
                { responseJsonrpc = "2.0"
                , responseResult = Just result
                , responseError = Nothing
                , responseId = requestId req
                }

-- | Handle tools/call request
handleCallTool :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleCallTool server req = do
    case requestParams req of
        Nothing -> return $ Just $ errorResponse (-32602) "Invalid params"
        Just p -> case fromJSON p of
            Error err -> return $ Just $ errorResponse (-32602) (T.pack err)
            Success (ToolCallArgs toolName args) -> do
                handlers <- readTVarIO (toolHandlers server)
                case Map.lookup toolName handlers of
                    Nothing -> return $ Just $ errorResponse (-32601) "Tool not found"
                    Just handler -> do
                        result <- handler args
                        return $
                            Just $
                                JSONRPCResponse
                                    { responseJsonrpc = "2.0"
                                    , responseResult = Just (toJSON result)
                                    , responseError = Nothing
                                    , responseId = requestId req
                                    }
  where
    errorResponse code msg =
        JSONRPCResponse
            { responseJsonrpc = "2.0"
            , responseResult = Nothing
            , responseError = Just $ JSONRPCError code msg Nothing
            , responseId = requestId req
            }

-- | Handle resources/list request
handleListResources :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleListResources server req = do
    resourcesList <- readTVarIO (resources server)
    let result = object ["resources" .= resourcesList]

    return $
        Just $
            JSONRPCResponse
                { responseJsonrpc = "2.0"
                , responseResult = Just result
                , responseError = Nothing
                , responseId = requestId req
                }

-- | Handle resources/read request
handleReadResource :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleReadResource server req = do
    case requestParams req of
        Nothing -> return $ Just $ errorResponse (-32602) "Invalid params"
        Just p -> case fromJSON p of
            Error err -> return $ Just $ errorResponse (-32602) (T.pack err)
            Success uri -> do
                handlers <- readTVarIO (resourceHandlers server)
                case Map.lookup uri handlers of
                    Nothing -> return $ Just $ errorResponse (-32601) "Resource not found"
                    Just handler -> do
                        content <- handler
                        return $
                            Just $
                                JSONRPCResponse
                                    { responseJsonrpc = "2.0"
                                    , responseResult = Just content
                                    , responseError = Nothing
                                    , responseId = requestId req
                                    }
  where
    errorResponse code msg =
        JSONRPCResponse
            { responseJsonrpc = "2.0"
            , responseResult = Nothing
            , responseError = Just $ JSONRPCError code msg Nothing
            , responseId = requestId req
            }
