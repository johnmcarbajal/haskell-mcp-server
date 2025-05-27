{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server (
    MCPServer (..),
    defaultServer,
    runMCPServer,
    runStdioServer,
    handleMessage,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception (IOException, catch, finally)
import Control.Monad (forever)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.WebSockets as WS
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Printf (printf)

import MCP.Types

-- * Server State

data MCPServer = MCPServer
    { serverInfo :: ServerInfo
    , tools :: TVar [Tool]
    , resources :: TVar [Resource]
    , toolHandlers :: TVar (Map.Map Text (Value -> IO ToolResult))
    , resourceHandlers :: TVar (Map.Map Text (IO Value))
    , initialized :: TVar Bool
    }

defaultServer :: IO MCPServer
defaultServer = MCPServer defaultServerInfo
    <$> newTVarIO []
    <*> newTVarIO []
    <*> newTVarIO Map.empty
    <*> newTVarIO Map.empty
    <*> newTVarIO False
  where
    defaultServerInfo = ServerInfo "haskell-mcp-server" "0.1.0" (MCPVersion 2024 11)

-- * Utilities

logInfo :: String -> IO ()
logInfo msg = hPutStrLn stderr msg >> hFlush stderr

-- * JSON-RPC Response Builders

mkResponse :: Maybe Value -> Maybe Value -> Maybe JSONRPCError -> JSONRPCResponse
mkResponse reqId result err = JSONRPCResponse "2.0" result err reqId

success :: ToJSON a => Maybe Value -> a -> JSONRPCResponse
success reqId = mkResponse reqId . Just . toJSON <*> pure Nothing

errorResponse :: Maybe Value -> Int -> Text -> JSONRPCResponse
errorResponse reqId code msg = mkResponse reqId Nothing (Just $ JSONRPCError code msg Nothing)

-- Error codes
parseError, methodNotFound, invalidParams :: Int
parseError = -32700
methodNotFound = -32601
invalidParams = -32602

-- * Server Runners

runMCPServer :: MCPServer -> Int -> IO ()
runMCPServer server port = do
    logInfo $ printf "Starting MCP Server on port %d" port
    logStartupInfo
    WS.runServer "127.0.0.1" port $ \pending -> do
        conn <- WS.acceptRequest pending
        logInfo "Client connected"
        WS.withPingThread conn 30 (pure ()) $
            handleClient server conn `finally` logInfo "Client disconnected"

runStdioServer :: MCPServer -> IO ()
runStdioServer server = do
    logInfo "Starting MCP Server in stdio mode"
    logStartupInfo
    logInfo "Server ready, waiting for messages..."
    
    messageLoop `catch` \(e :: IOException) ->
        logInfo $ "Connection ended: " ++ show e
  where
    messageLoop = forever $ do
        logInfo "Waiting for next message..."
        race (threadDelay 5000000) getLine >>= \case
            Left _ -> logInfo "Timeout - client may have disconnected"
            Right line -> processMessage server line

logStartupInfo :: IO ()
logStartupInfo = do
    logInfo "Available tools: echo, current_time, calculate, random_number, text_analysis, weather, generate_uuid, base64, jeff_invite_predictor"
    logInfo "Available resources: config://server.json"

processMessage :: MCPServer -> String -> IO ()
processMessage server line = do
    logInfo $ "Received: " ++ take 100 line ++ "..."
    case eitherDecode (L8.pack line) of
        Left err -> logInfo $ "Parse error: " ++ err
        Right req -> do
            logInfo $ "Processing: " ++ T.unpack (requestMethod req)
            handleMessage server req >>= \case
                Just resp -> do
                    let respStr = take 100 $ L8.unpack $ encode resp
                    logInfo $ "Sending: " ++ respStr ++ "..."
                    L8.putStrLn (encode resp) >> hFlush stdout
                    logInfo $ "Response sent for: " ++ T.unpack (requestMethod req)
                Nothing -> logInfo $ "No response for: " ++ T.unpack (requestMethod req)

handleClient :: MCPServer -> WS.Connection -> IO ()
handleClient server conn = forever $ do
    msg <- WS.receiveData conn
    case eitherDecode msg of
        Left err -> do
            logInfo $ "Decode error: " ++ err
            WS.sendTextData conn . encode $ errorResponse Nothing parseError "Parse error"
        Right req -> 
            handleMessage server req >>= mapM_ (WS.sendTextData conn . encode)

-- * Message Handling

handleMessage :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleMessage server req@JSONRPCRequest{requestMethod, requestId} = 
    case requestMethod of
        "initialize" -> handleInitialize server req
        "initialized" -> handleInitialized server >> pure Nothing
        "tools/list" -> Just . success requestId . object . pure . ("tools" .=) <$> readTVarIO (tools server)
        "tools/call" -> handleToolCall server req
        "resources/list" -> Just . success requestId . object . pure . ("resources" .=) <$> readTVarIO (resources server)
        "resources/read" -> handleResourceRead server req
        _ -> pure . Just $ errorResponse requestId methodNotFound "Method not found"

handleInitialize :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleInitialize _ JSONRPCRequest{requestParams, requestId} = do
    logInfo "Handling initialize request"
    case requestParams >>= parseMaybe parseJSON of
        Nothing -> do
            logInfo "Failed to parse initialize params"
            pure . Just $ errorResponse requestId invalidParams "Invalid params"
        Just InitializeParams{initProtocolVersion, initClientInfo} -> do
            logInfo $ "Client version: " ++ show initProtocolVersion
            logInfo $ "Client info: " ++ show initClientInfo
            logInfo "Sending initialize response"
            pure . Just $ success requestId initializeResponse
  where
    initializeResponse = object
        [ "protocolVersion" .= ("2024-11-05" :: Text)
        , "capabilities" .= object
            [ "tools" .= object ["listChanged" .= True]
            , "resources" .= object ["subscribe" .= False, "listChanged" .= True]
            ]
        , "serverInfo" .= object
            [ "name" .= ("haskell-mcp-server" :: Text)
            , "version" .= ("0.1.0" :: Text)
            ]
        ]

handleInitialized :: MCPServer -> IO ()
handleInitialized MCPServer{initialized} = do
    atomically $ writeTVar initialized True
    logInfo "Server initialized"

handleToolCall :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleToolCall MCPServer{toolHandlers} JSONRPCRequest{requestParams, requestId} =
    case requestParams >>= parseMaybe parseJSON of
        Nothing -> pure . Just $ errorResponse requestId invalidParams "Invalid params"
        Just ToolCallArgs{callName, callArguments} -> do
            handlers <- readTVarIO toolHandlers
            case Map.lookup callName handlers of
                Nothing -> pure . Just $ errorResponse requestId methodNotFound "Tool not found"
                Just handler -> Just . success requestId <$> handler callArguments

handleResourceRead :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleResourceRead MCPServer{resourceHandlers} JSONRPCRequest{requestParams, requestId} =
    case requestParams >>= parseMaybe parseJSON of
        Nothing -> pure . Just $ errorResponse requestId invalidParams "Invalid params"
        Just uri -> do
            handlers <- readTVarIO resourceHandlers
            case Map.lookup uri handlers of
                Nothing -> pure . Just $ errorResponse requestId methodNotFound "Resource not found"
                Just handler -> Just . success requestId <$> handler