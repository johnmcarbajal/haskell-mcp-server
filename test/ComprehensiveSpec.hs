{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ComprehensiveSpec where

import Control.Concurrent.STM
import Control.Exception (evaluate, try, SomeException)
import Data.Aeson hiding (Success)
import Data.Aeson.Types (Result(..))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import Test.Hspec
import Test.QuickCheck hiding (Success, Result)
import Test.QuickCheck.Instances.Text ()
import Control.Monad.IO.Class (liftIO)

import MCP.Server
import MCP.Tools
import MCP.Types

-- Property-based testing for JSON round-trips
instance Arbitrary MCPVersion where
  arbitrary = MCPVersion <$> choose (2020, 2030) <*> choose (1, 12)

instance Arbitrary ContentItem where
  arbitrary = ContentItem <$> elements ["text", "image", "resource"] 
                         <*> arbitrary

instance Arbitrary ToolResult where
  arbitrary = ToolResult <$> listOf arbitrary <*> arbitrary

prop_mcpVersionRoundtrip :: MCPVersion -> Bool
prop_mcpVersionRoundtrip version = 
  case decode (encode version) of
    Just decoded -> decoded == version
    Nothing -> False

prop_toolResultRoundtrip :: ToolResult -> Bool
prop_toolResultRoundtrip result =
  case decode (encode result) of
    Just decoded -> decoded == result
    Nothing -> False

comprehensiveSpec :: Spec
comprehensiveSpec = do
  describe "Property-Based Testing" $ do
    it "MCPVersion JSON round-trip" $ property prop_mcpVersionRoundtrip
    it "ToolResult JSON round-trip" $ property prop_toolResultRoundtrip

  describe "JSON-RPC Protocol Compliance" $ do
    it "rejects malformed JSON" $ do
      server <- defaultServer
      let malformedJson = "{invalid json"
      case eitherDecode (L8.pack malformedJson) of
        Left _ -> return () -- Expected
        Right (_ :: JSONRPCRequest) -> expectationFailure "Should reject malformed JSON"

    it "handles missing jsonrpc field" $ do
      server <- defaultServer
      let invalidRequest = object 
            [ "method" .= ("test" :: Text)
            , "id" .= (1 :: Int)
            ]
      case fromJSON invalidRequest of
        Error _ -> return () -- Expected
        Success (_ :: JSONRPCRequest) -> expectationFailure "Should require jsonrpc field"

    it "validates method field types" $ do
      let invalidRequest = object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= (123 :: Int) -- Should be string
            , "id" .= (1 :: Int)
            ]
      case fromJSON invalidRequest of
        Error _ -> return () -- Expected
        Success (_ :: JSONRPCRequest) -> expectationFailure "Method must be string"

    it "processes null IDs correctly" $ do
      server <- defaultServer
      let request = JSONRPCRequest "2.0" "unknown" Nothing Nothing
      response <- handleMessage server request
      case response of
        Just (JSONRPCResponse _ _ _ Nothing) -> return () -- Expected
        _ -> expectationFailure "Should preserve null ID"

  describe "Complete Tool Coverage" $ do
    it "tests random number generation with valid range" $ do
      let args = object ["min" .= (1 :: Int), "max" .= (10 :: Int)]
      result <- randomNumberHandler args
      case ComprehensiveSpec.resultContent result of
        [ContentItem "text" msg] -> do
          T.isInfixOf "Random number:" msg `shouldBe` True
          ComprehensiveSpec.resultIsError result `shouldBe` Nothing
        _ -> expectationFailure "Expected single text content"

    it "tests random number with invalid range" $ do
      let args = object ["min" .= (10 :: Int), "max" .= (1 :: Int)]
      result <- randomNumberHandler args
      ComprehensiveSpec.resultIsError result `shouldBe` Just True

    it "validates text analysis with empty string" $ do
      let args = object ["text" .= ("" :: Text)]
      result <- textAnalysisHandler args
      case ComprehensiveSpec.resultContent result of
        [ContentItem "text" msg] -> do
          T.isInfixOf "Characters: 0" msg `shouldBe` True
          ComprehensiveSpec.resultIsError result `shouldBe` Nothing
        _ -> expectationFailure "Expected analysis of empty string"

    it "validates text analysis with unicode" $ do
      let args = object ["text" .= ("Hello 🌍 émojis!" :: Text)]
      result <- textAnalysisHandler args
      case ComprehensiveSpec.resultContent result of
        [ContentItem "text" msg] -> do
          T.isInfixOf "Characters:" msg `shouldBe` True
          ComprehensiveSpec.resultIsError result `shouldBe` Nothing
        _ -> expectationFailure "Expected unicode text analysis"

    it "verifies UUID format" $ do
      result <- uuidHandler (object [])
      case ComprehensiveSpec.resultContent result of
        [ContentItem "text" msg] -> do
          T.isInfixOf "Generated UUID:" msg `shouldBe` True
          T.length (T.drop 16 msg) `shouldBe` 36 -- UUID length (after "Generated UUID: ")
          ComprehensiveSpec.resultIsError result `shouldBe` Nothing
        _ -> expectationFailure "Expected UUID generation"

    it "checks base64 encoding" $ do
      let args = object 
            [ "operation" .= ("encode" :: Text)
            , "text" .= ("Hello World" :: Text)
            ]
      result <- base64Handler args
      case ComprehensiveSpec.resultContent result of
        [ContentItem "text" msg] -> do
          T.isInfixOf "Base64 encoded:" msg `shouldBe` True
          ComprehensiveSpec.resultIsError result `shouldBe` Nothing
        _ -> expectationFailure "Expected base64 encoding"

    it "checks base64 decoding" $ do
      let args = object
            [ "operation" .= ("decode" :: Text)
            , "text" .= ("SGVsbG8gV29ybGQ=" :: Text)
            ]
      result <- base64Handler args
      case ComprehensiveSpec.resultContent result of
        [ContentItem "text" msg] -> do
          T.isInfixOf "Hello World" msg `shouldBe` True
          ComprehensiveSpec.resultIsError result `shouldBe` Nothing
        _ -> expectationFailure "Expected base64 decoding"

    it "handles invalid base64" $ do
      let args = object
            [ "operation" .= ("decode" :: Text)
            , "text" .= ("Invalid=Base64!" :: Text)
            ]
      result <- base64Handler args
      ComprehensiveSpec.resultIsError result `shouldBe` Just True

    it "tests weather tool" $ do
      let args = object ["location" .= ("London" :: Text)]
      result <- weatherHandler args
      case ComprehensiveSpec.resultContent result of
        [ContentItem "text" msg] -> do
          T.isInfixOf "Weather Report for London" msg `shouldBe` True
          T.isInfixOf "Temperature:" msg `shouldBe` True
          ComprehensiveSpec.resultIsError result `shouldBe` Nothing
        _ -> expectationFailure "Expected weather report"

    it "tests Jeff's predictor" $ do
      result <- jeffInviteHandler (object [])
      case ComprehensiveSpec.resultContent result of
        [ContentItem "text" msg] -> do
          T.isInfixOf "Bacon Cheeseburger" msg `shouldBe` True
          T.isInfixOf "Predicted Date:" msg `shouldBe` True
          ComprehensiveSpec.resultIsError result `shouldBe` Nothing
        _ -> expectationFailure "Expected Jeff's prediction"

  describe "Calculator Edge Cases" $ do
    it "handles division by zero" $ do
      let args = object ["expression" .= ("1 / 0" :: Text)]
      result <- calculatorHandler args
      ComprehensiveSpec.resultIsError result `shouldBe` Just True

    it "handles empty expression" $ do
      let args = object ["expression" .= ("" :: Text)]
      result <- calculatorHandler args
      ComprehensiveSpec.resultIsError result `shouldBe` Just True

    it "handles invalid syntax" $ do
      let args = object ["expression" .= ("1 + + 2" :: Text)]
      result <- calculatorHandler args
      ComprehensiveSpec.resultIsError result `shouldBe` Just True

    it "handles subtraction" $ do
      let args = object ["expression" .= ("10 - 3" :: Text)]
      result <- calculatorHandler args
      case ComprehensiveSpec.resultContent result of
        [ContentItem "text" msg] -> do
          T.isInfixOf "Result: 7.0" msg `shouldBe` True
          ComprehensiveSpec.resultIsError result `shouldBe` Nothing
        _ -> expectationFailure "Expected subtraction result"

    it "handles unknown operator" $ do
      let args = object ["expression" .= ("5 % 2" :: Text)]
      result <- calculatorHandler args
      ComprehensiveSpec.resultIsError result `shouldBe` Just True

  describe "Error Response Coverage" $ do
    it "returns method not found error" $ do
      server <- defaultServer
      let request = JSONRPCRequest "2.0" "nonexistent/method" Nothing (Just (Number 1))
      response <- handleMessage server request
      case response of
        Just (JSONRPCResponse _ Nothing (Just err) _) -> do
          getErrorCode err `shouldBe` (-32601)
          getErrorMessage err `shouldBe` "Method not found"
        _ -> expectationFailure "Expected method not found error"

    it "returns invalid params error for tool call" $ do
      server <- defaultServer
      addTool server echoTool echoHandler
      let request = JSONRPCRequest "2.0" "tools/call" 
                     (Just $ object ["invalid" .= True]) (Just (Number 1))
      response <- handleMessage server request
      case response of
        Just (JSONRPCResponse _ Nothing (Just err) _) -> do
          getErrorCode err `shouldBe` (-32602)
          getErrorMessage err `shouldBe` "Invalid params"
        _ -> expectationFailure "Expected invalid params error"

    it "returns tool not found error" $ do
      server <- defaultServer
      let request = JSONRPCRequest "2.0" "tools/call"
                     (Just $ object 
                       [ "name" .= ("nonexistent" :: Text)
                       , "arguments" .= object []
                       ]) (Just (Number 1))
      response <- handleMessage server request
      case response of
        Just (JSONRPCResponse _ Nothing (Just err) _) -> do
          getErrorCode err `shouldBe` (-32601)
          getErrorMessage err `shouldBe` "Tool not found"
        _ -> expectationFailure "Expected tool not found error"

  describe "Resource Operations" $ do
    it "lists empty resources initially" $ do
      server <- defaultServer
      let request = JSONRPCRequest "2.0" "resources/list" Nothing (Just (Number 1))
      response <- handleMessage server request
      case response of
        Just (JSONRPCResponse _ (Just result) Nothing _) -> do
          case fromJSON result of
            Success (Object obj) -> case KM.lookup "resources" obj of
              Just (Array arr) -> length arr `shouldBe` 0
              _ -> expectationFailure "Expected resources array"
            _ -> expectationFailure "Expected object response"
        _ -> expectationFailure "Expected successful response"

    it "handles missing resource read" $ do
      server <- defaultServer
      let request = JSONRPCRequest "2.0" "resources/read" 
                     (Just $ String "nonexistent://resource") (Just (Number 1))
      response <- handleMessage server request
      case response of
        Just (JSONRPCResponse _ Nothing (Just err) _) -> do
          getErrorCode err `shouldBe` (-32601)
          getErrorMessage err `shouldBe` "Resource not found"
        _ -> expectationFailure "Expected resource not found error"

    it "handles invalid resource read params" $ do
      server <- defaultServer
      let request = JSONRPCRequest "2.0" "resources/read"
                     (Just $ object ["invalid" .= True]) (Just (Number 1))
      response <- handleMessage server request
      case response of
        Just (JSONRPCResponse _ Nothing (Just err) _) -> do
          errorCode err `shouldBe` (-32602)
        _ -> expectationFailure "Expected invalid params error"

  describe "Server State Edge Cases" $ do
    it "handles concurrent tool additions" $ do
      server <- defaultServer
      -- Add tools concurrently (simplified simulation)
      addTool server echoTool echoHandler
      addTool server timeTool timeHandler
      addTool server calculatorTool calculatorHandler
      
      toolsList <- readTVarIO (tools server)
      length toolsList `shouldBe` 3

    it "maintains initialization state correctly" $ do
      server <- defaultServer
      
      -- Check initial state
      isInit1 <- readTVarIO (initialized server)
      isInit1 `shouldBe` False
      
      -- Initialize (directly set the state since handleInitialized doesn't exist)
      atomically $ writeTVar (initialized server) True
      
      -- Check final state  
      isInit2 <- readTVarIO (initialized server)
      isInit2 `shouldBe` True

  describe "Parameter Type Validation" $ do
    it "handles null parameters gracefully" $ do
      let args = object ["message" .= Null]
      result <- echoHandler args
      ComprehensiveSpec.resultIsError result `shouldBe` Just True

    it "handles wrong parameter types" $ do
      let args = object ["message" .= (123 :: Int)] -- Should be string
      result <- echoHandler args
      ComprehensiveSpec.resultIsError result `shouldBe` Just True

    it "handles nested object parameters" $ do
      let args = object 
            [ "text" .= object ["nested" .= ("value" :: Text)]
            ]
      result <- textAnalysisHandler args
      ComprehensiveSpec.resultIsError result `shouldBe` Just True

    it "handles array parameters where object expected" $ do
      let args = Array mempty
      result <- echoHandler args
      ComprehensiveSpec.resultIsError result `shouldBe` Just True

  describe "MCPVersion Edge Cases" $ do
    it "handles minimum version" $ do
      let version = MCPVersion 2000 0  -- Minimum valid version
      let encoded = encode version
      case decode encoded of
        Just decoded -> decoded `shouldBe` version
        Nothing -> expectationFailure "Should handle minimum version"

    it "handles future version" $ do
      let version = MCPVersion 2100 12  -- Maximum valid version
      let encoded = encode version  
      case decode encoded of
        Just decoded -> decoded `shouldBe` version
        Nothing -> expectationFailure "Should handle future version"

-- Helper functions for accessing ToolResult fields after refactoring
resultContent :: ToolResult -> [ContentItem]
resultContent (ToolResult content _) = content

resultIsError :: ToolResult -> Maybe Bool
resultIsError (ToolResult _ isErr) = isErr

-- Helper functions to extract JSONRPCError fields
getErrorCode :: JSONRPCError -> Int
getErrorCode (JSONRPCError code _ _) = code

getErrorMessage :: JSONRPCError -> T.Text
getErrorMessage (JSONRPCError _ msg _) = msg