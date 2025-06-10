{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Data.Aeson
import qualified Data.Text as T
import Test.Hspec

import MCP.Server
import MCP.Tools
import MCP.Types
import qualified MCP.VersionSpec
import qualified MCP.VersionCompatibilitySpec
import qualified ComprehensiveSpec
import qualified PropertySpec
import qualified MCP.Version.ValidationSpec

-- Helper function to extract tool name from Tool
getToolName :: Tool -> T.Text
getToolName (Tool name _ _) = name

-- Helper functions to extract ServerInfo fields
getServerName :: ServerInfo -> T.Text
getServerName (ServerInfo name _ _) = name

getServerVersion :: ServerInfo -> T.Text
getServerVersion (ServerInfo _ version _) = version

getServerProtocolVersion :: ServerInfo -> MCPVersion
getServerProtocolVersion (ServerInfo _ _ pv) = pv

-- Helper functions to extract JSONRPCError fields
getErrorCode :: JSONRPCError -> Int
getErrorCode (JSONRPCError code _ _) = code

getErrorMessage :: JSONRPCError -> T.Text
getErrorMessage (JSONRPCError _ msg _) = msg

-- Helper functions to extract ToolResult fields
getToolResultContent :: ToolResult -> [ContentItem]
getToolResultContent (ToolResult content _) = content

getToolResultIsError :: ToolResult -> Maybe Bool
getToolResultIsError (ToolResult _ isErr) = isErr

main :: IO ()
main = hspec $ do
    MCP.VersionSpec.spec
    MCP.VersionCompatibilitySpec.spec
    MCP.Version.ValidationSpec.spec
    ComprehensiveSpec.comprehensiveSpec
    PropertySpec.propertySpec
    
    describe "MCP Types" $ do
        it "serializes and deserializes JSONRPCRequest" $ do
            let request =
                    JSONRPCRequest
                        { requestJsonrpc = "2.0"
                        , requestMethod = "test"
                        , requestParams = Just (String "test-params")
                        , requestId = Just (Number 1)
                        }
            let encoded = encode request
            let decoded = decode encoded
            decoded `shouldBe` Just request

        it "serializes and deserializes Tool" $ do
            let tool =
                    Tool
                        "test-tool"
                        "Test tool description"
                        (object ["type" .= ("object" :: T.Text)])
            let encoded = encode tool
            let decoded = decode encoded
            decoded `shouldBe` Just tool

        it "serializes and deserializes ServerInfo" $ do
            let serverInfo =
                    ServerInfo
                        "test-server"
                        "1.0.0"
                        (MCPVersion 2024 11)
            let encoded = encode serverInfo
            let decoded = decode encoded
            decoded `shouldBe` Just serverInfo

    describe "MCP Server" $ do
        it "creates default server configuration" $ do
            server <- defaultServer
            let info = serverInfo server
            getServerName info `shouldBe` "haskell-mcp-server"
            getServerVersion info `shouldBe` "0.1.0"
            getServerProtocolVersion info `shouldBe` MCPVersion 2024 11

        it "handles initialize request" $ do
            server <- defaultServer
            let request =
                    JSONRPCRequest
                        { requestJsonrpc = "2.0"
                        , requestMethod = "initialize"
                        , requestParams =
                            Just $
                                object
                                    [ "protocolVersion" .= MCPVersion 2024 11
                                    , "capabilities" .= object []
                                    , "clientInfo"
                                        .= object
                                            [ "name" .= ("test-client" :: T.Text)
                                            , "version" .= ("1.0.0" :: T.Text)
                                            ]
                                    ]
                        , requestId = Just (Number 1)
                        }

            response <- handleMessage server request
            response `shouldSatisfy` \case
                Just (JSONRPCResponse "2.0" (Just _) Nothing (Just (Number 1))) -> True
                _ -> False

        it "handles tools/list request" $ do
            server <- defaultServer
            addTool server echoTool echoHandler

            let request =
                    JSONRPCRequest
                        { requestJsonrpc = "2.0"
                        , requestMethod = "tools/list"
                        , requestParams = Nothing
                        , requestId = Just (Number 2)
                        }

            response <- handleMessage server request
            response `shouldSatisfy` \case
                Just (JSONRPCResponse "2.0" (Just _) Nothing (Just (Number 2))) -> True
                _ -> False

        it "handles unknown method" $ do
            server <- defaultServer
            let request =
                    JSONRPCRequest
                        { requestJsonrpc = "2.0"
                        , requestMethod = "unknown/method"
                        , requestParams = Nothing
                        , requestId = Just (Number 3)
                        }

            response <- handleMessage server request
            response `shouldSatisfy` \case
                Just (JSONRPCResponse "2.0" Nothing (Just err) (Just (Number 3))) ->
                    getErrorCode err == -32601 && getErrorMessage err == "Method not found"
                _ -> False

    describe "MCP Tools" $ do
        it "echo tool handler works correctly" $ do
            let args = object ["message" .= ("Hello, World!" :: T.Text)]
            result <- echoHandler args

            case getToolResultContent result of
                [ContentItem "text" msg] -> msg `shouldBe` "Echo: Hello, World!"
                _ -> expectationFailure "Unexpected content structure"
            getToolResultIsError result `shouldBe` Nothing

        it "echo tool handler handles missing parameters" $ do
            let args = object []
            result <- echoHandler args

            getToolResultIsError result `shouldBe` Just True

        it "time tool handler returns current time" $ do
            result <- timeHandler (object [])

            case getToolResultContent result of
                [ContentItem "text" msg] -> T.isPrefixOf "Current time:" msg `shouldBe` True
                _ -> expectationFailure "Unexpected content structure"
            getToolResultIsError result `shouldBe` Nothing

        it "calculator tool performs addition" $ do
            let args = object ["expression" .= ("2 + 3" :: T.Text)]
            result <- calculatorHandler args

            case getToolResultContent result of
                [ContentItem "text" msg] -> msg `shouldBe` "Result: 5.0"
                _ -> expectationFailure "Unexpected content structure"
            getToolResultIsError result `shouldBe` Nothing

        it "calculator tool handles multiplication" $ do
            let args = object ["expression" .= ("4 * 5" :: T.Text)]
            result <- calculatorHandler args

            case getToolResultContent result of
                [ContentItem "text" msg] -> msg `shouldBe` "Result: 20.0"
                _ -> expectationFailure "Unexpected content structure"
            getToolResultIsError result `shouldBe` Nothing

        it "calculator tool handles division" $ do
            let args = object ["expression" .= ("10 / 2" :: T.Text)]
            result <- calculatorHandler args

            case getToolResultContent result of
                [ContentItem "text" msg] -> msg `shouldBe` "Result: 5.0"
                _ -> expectationFailure "Unexpected content structure"
            getToolResultIsError result `shouldBe` Nothing

        it "calculator tool handles invalid expressions" $ do
            let args = object ["expression" .= ("invalid expression" :: T.Text)]
            result <- calculatorHandler args

            getToolResultIsError result `shouldBe` Just True

        it "calculator tool handles missing parameters" $ do
            let args = object []
            result <- calculatorHandler args

            getToolResultIsError result `shouldBe` Just True

    describe "Server State Management" $ do
        it "allows adding and retrieving tools" $ do
            server <- defaultServer
            addTool server echoTool echoHandler

            toolsList <- readTVarIO (tools server)
            length toolsList `shouldBe` 1
            getToolName (head toolsList) `shouldBe` "echo"

        it "allows adding multiple tools" $ do
            server <- defaultServer
            addTool server echoTool echoHandler
            addTool server timeTool timeHandler
            addTool server calculatorTool calculatorHandler

            toolsList <- readTVarIO (tools server)
            length toolsList `shouldBe` 3

        it "manages server initialization state" $ do
            server <- defaultServer
            isInit <- readTVarIO (initialized server)
            isInit `shouldBe` False

            atomically $ writeTVar (initialized server) True
            isInitAfter <- readTVarIO (initialized server)
            isInitAfter `shouldBe` True
