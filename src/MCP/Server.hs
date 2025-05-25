{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Server (
    MCPServer (..),
    defaultServer,
    runMCPServer,
    handleMessage,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Network.WebSockets as WS

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

-- | Run the MCP Server
runMCPServer :: MCPServer -> Int -> IO ()
runMCPServer server port = do
    putStrLn $ "Starting MCP Server on port " ++ show port
    WS.runServer "127.0.0.1" port $ \pending -> do
        conn <- WS.acceptRequest pending
        putStrLn "Client connected"
        WS.withPingThread conn 30 (return ()) $ do
            handleClient server conn

-- | Handle client connection
handleClient :: MCPServer -> WS.Connection -> IO ()
handleClient server conn = flip finally disconnect $ do
    putStrLn "Handling client messages..."
    forever $ do
        msg <- WS.receiveData conn
        case eitherDecode msg of
            Left err -> do
                putStrLn $ "Failed to decode message: " ++ err
                sendError conn Nothing (-32700) "Parse error"
            Right request -> do
                response <- handleMessage server request
                case response of
                    Just resp -> WS.sendTextData conn (encode resp)
                    Nothing -> return ()
  where
    disconnect = putStrLn "Client disconnected"

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
    let result =
            object
                [ "protocolVersion" .= serverProtocolVersion (serverInfo server)
                , "capabilities"
                    .= object
                        [ "tools" .= object ["listChanged" .= True]
                        , "resources" .= object ["subscribe" .= False, "listChanged" .= True]
                        ]
                , "serverInfo" .= serverInfo server
                ]

    return $
        Just $
            JSONRPCResponse
                { responseJsonrpc = "2.0"
                , responseResult = Just result
                , responseError = Nothing
                , responseId = requestId req
                }

-- | Handle initialized notification
handleInitialized :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleInitialized server _ = do
    atomically $ writeTVar (initialized server) True
    putStrLn "Server initialized"
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
