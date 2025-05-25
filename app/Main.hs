{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.Text as T
import System.Environment (getArgs)
import Text.Read (readMaybe)

import MCP.Server
import MCP.Tools
import MCP.Types

main :: IO ()
main = do
    args <- getArgs
    let port = case args of
            (p : _) -> case readMaybe p of
                Just n -> n
                Nothing -> 3000
            [] -> 3000

    -- Create server instance
    server <- defaultServer

    -- Add example tools
    addTool server echoTool echoHandler
    addTool server timeTool timeHandler
    addTool server calculatorTool calculatorHandler

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
                        , "port" .= port
                        ]
                , "tools_count" .= (3 :: Int)
                ]

    -- Start the server
    putStrLn $ "Starting Haskell MCP Server on port " ++ show port
    putStrLn "Available tools: echo, current_time, calculate"
    putStrLn "Available resources: config://server.json"
    runMCPServer server port
