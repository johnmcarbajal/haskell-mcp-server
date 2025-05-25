{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.Text as T
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import MCP.Server
import MCP.Tools
import MCP.Types

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--stdio"] -> runInStdioMode
        [] -> runInWebSocketMode 3000
        [portStr] -> case readMaybe portStr of
            Just port -> runInWebSocketMode port
            Nothing -> do
                hPutStrLn stderr "Invalid port number. Usage: haskell-mcp-server-exe [port] or haskell-mcp-server-exe --stdio"
                hPutStrLn stderr "Examples:"
                hPutStrLn stderr "  haskell-mcp-server-exe --stdio    # Run in stdio mode for Claude Desktop"
                hPutStrLn stderr "  haskell-mcp-server-exe            # Run WebSocket server on port 3000"
                hPutStrLn stderr "  haskell-mcp-server-exe 8080       # Run WebSocket server on port 8080"
        _ -> do
            hPutStrLn stderr "Usage: haskell-mcp-server-exe [--stdio | port]"
            hPutStrLn stderr "  --stdio: Run in stdio mode for Claude Desktop integration"
            hPutStrLn stderr "  port:    Run WebSocket server on specified port (default: 3000)"

runInStdioMode :: IO ()
runInStdioMode = do
    server <- setupServer
    runStdioServer server

runInWebSocketMode :: Int -> IO ()
runInWebSocketMode port = do
    server <- setupServer
    hPutStrLn stderr $ "Starting Haskell MCP Server on port " ++ show port
    hPutStrLn stderr "Available tools: echo, current_time, calculate, random_number, text_analysis, weather, generate_uuid, base64"
    hPutStrLn stderr "Available resources: config://server.json"
    hPutStrLn stderr ""
    hPutStrLn stderr "For Claude Desktop integration, use: haskell-mcp-server-exe --stdio"
    runMCPServer server port

setupServer :: IO MCPServer
setupServer = do
    -- Create server instance
    server <- defaultServer

    -- Add original tools
    addTool server echoTool echoHandler
    addTool server timeTool timeHandler
    addTool server calculatorTool calculatorHandler

    -- Add new custom tools
    addTool server randomNumberTool randomNumberHandler
    addTool server textAnalysisTool textAnalysisHandler
    addTool server weatherTool weatherHandler
    addTool server uuidTool uuidHandler
    addTool server base64Tool base64Handler

    -- Add example resource
    let exampleResource =
            Resource
                "config://server.json"
                "Server Configuration"
                (Just "Current server configuration")
                (Just "application/json")

    addResource server exampleResource $
        return $
            object
                [ "server"
                    .= object
                        [ "name" .= ("haskell-mcp-server" :: T.Text)
                        , "version" .= ("0.1.0" :: T.Text)
                        , "mode" .= ("stdio and websocket" :: T.Text)
                        ]
                , "tools_count" .= (8 :: Int)
                , "capabilities"
                    .= object
                        [ "stdio" .= True
                        , "websocket" .= True
                        ]
                , "available_tools"
                    .= [ "echo" :: T.Text
                       , "current_time"
                       , "calculate"
                       , "random_number"
                       , "text_analysis"
                       , "weather"
                       , "generate_uuid"
                       , "base64"
                       ]
                ]

    return server
