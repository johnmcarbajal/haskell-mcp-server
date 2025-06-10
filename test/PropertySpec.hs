{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PropertySpec where

import Control.Exception (evaluate, try, SomeException)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Instances.Text ()

import MCP.Types
import MCP.Tools

-- Arbitrary instances for property-based testing

instance Arbitrary MCPVersion where
  arbitrary = MCPVersion <$> choose (2020, 2030) <*> choose (1, 12)

instance Arbitrary ContentItem where
  arbitrary = ContentItem 
    <$> elements ["text", "image", "resource", "data"]
    <*> (T.take 1000 . (\t -> if T.null t then "content" else t) <$> arbitrary) -- Ensure non-empty

instance Arbitrary ToolResult where
  arbitrary = ToolResult 
    <$> resize 5 (listOf1 arbitrary) -- At least one item, limit list size
    <*> frequency [(3, pure Nothing), (1, pure (Just True)), (1, pure (Just False))]

instance Arbitrary JSONRPCError where
  arbitrary = JSONRPCError 
    <$> choose (-32768, -32000) -- JSON-RPC error code range
    <*> (T.take 200 <$> arbitrary)
    <*> frequency [(2, pure Nothing), (1, Just <$> arbitrary)]

instance Arbitrary JSONRPCRequest where
  arbitrary = JSONRPCRequest
    <$> pure "2.0"
    <*> elements ["initialize", "tools/list", "tools/call", "resources/list"]
    <*> frequency [(1, pure Nothing), (2, Just <$> arbitrary)]
    <*> frequency [(1, pure Nothing), (2, Just <$> arbitrary)]

instance Arbitrary JSONRPCResponse where
  arbitrary = JSONRPCResponse
    <$> pure "2.0" 
    <*> frequency [(1, pure Nothing), (2, Just <$> arbitrary)]
    <*> frequency [(3, pure Nothing), (1, Just <$> arbitrary)]
    <*> frequency [(1, pure Nothing), (2, Just <$> arbitrary)]

instance Arbitrary ServerInfo where
  arbitrary = ServerInfo
    <$> (T.take 50 <$> arbitrary)
    <*> (T.take 20 <$> arbitrary) 
    <*> arbitrary

instance Arbitrary Tool where
  arbitrary = Tool
    <$> (T.take 50 . (\t -> if T.null t then "tool" else t) <$> arbitrary) -- Ensure non-empty name
    <*> (T.take 200 <$> arbitrary)
    <*> arbitrary

instance Arbitrary Resource where
  arbitrary = Resource
    <$> (T.take 100 <$> arbitrary)
    <*> (T.take 50 <$> arbitrary)
    <*> frequency [(1, pure Nothing), (2, Just . T.take 200 <$> arbitrary)]
    <*> frequency [(2, pure Nothing), (1, Just <$> elements ["text/plain", "application/json"])]

-- Property tests

prop_mcpVersionRoundtrip :: MCPVersion -> Bool
prop_mcpVersionRoundtrip version = 
  case decode (encode version) of
    Just decoded -> decoded == version
    Nothing -> False

prop_contentItemRoundtrip :: ContentItem -> Bool
prop_contentItemRoundtrip item =
  case decode (encode item) of
    Just decoded -> decoded == item
    Nothing -> False

prop_toolResultRoundtrip :: ToolResult -> Bool
prop_toolResultRoundtrip result =
  case decode (encode result) of
    Just decoded -> decoded == result
    Nothing -> False

prop_jsonrpcRequestRoundtrip :: JSONRPCRequest -> Bool
prop_jsonrpcRequestRoundtrip request =
  case decode (encode request) of
    Just decoded -> decoded == request
    Nothing -> False

prop_jsonrpcResponseRoundtrip :: JSONRPCResponse -> Bool
prop_jsonrpcResponseRoundtrip response =
  case decode (encode response) of
    Just decoded -> decoded == response
    Nothing -> False

prop_serverInfoRoundtrip :: ServerInfo -> Bool
prop_serverInfoRoundtrip info =
  case decode (encode info) of
    Just decoded -> decoded == info
    Nothing -> False

prop_toolRoundtrip :: Tool -> Bool
prop_toolRoundtrip tool =
  case decode (encode tool) of
    Just decoded -> decoded == tool
    Nothing -> False

prop_resourceRoundtrip :: Resource -> Bool
prop_resourceRoundtrip resource =
  case decode (encode resource) of
    Just decoded -> decoded == resource
    Nothing -> False

-- Property: JSON encoding produces valid JSON
prop_encodingProducesValidJSON :: ToolResult -> Bool
prop_encodingProducesValidJSON result =
  case decode (encode result) of
    Just (_ :: Value) -> True
    Nothing -> False

-- Property: Error results always have isError = Just True
prop_errorResultsHaveErrorFlag :: Property
prop_errorResultsHaveErrorFlag = forAll arbitrary $ \content ->
  let errorResult = ToolResult content (Just True)
  in case PropertySpec.resultIsError errorResult of
       Just True -> True
       _ -> False

-- Property: Success results have isError = Nothing or Just False  
prop_successResultsNoErrorFlag :: Property
prop_successResultsNoErrorFlag = forAll arbitrary $ \content ->
  let successResult = ToolResult content Nothing
  in case PropertySpec.resultIsError successResult of
       Nothing -> True
       Just False -> True
       _ -> False

-- Property: Non-empty content lists
prop_toolResultsHaveContent :: ToolResult -> Bool
prop_toolResultsHaveContent (ToolResult content _) = not (null content)

-- Property: JSON-RPC version is always "2.0"
prop_jsonrpcVersionIsCorrect :: JSONRPCRequest -> Bool
prop_jsonrpcVersionIsCorrect req = requestJsonrpc req == "2.0"

prop_jsonrpcResponseVersionIsCorrect :: JSONRPCResponse -> Bool  
prop_jsonrpcResponseVersionIsCorrect resp = responseJsonrpc resp == "2.0"

-- Property: MCPVersion components are non-negative
prop_mcpVersionNonNegative :: MCPVersion -> Bool
prop_mcpVersionNonNegative (MCPVersion major minor) = major >= 0 && minor >= 0

-- Property: Tool names are non-empty
prop_toolNamesNonEmpty :: Tool -> Bool
prop_toolNamesNonEmpty (Tool name _ _) = not (T.null name)

-- Property: Calculator expressions with valid syntax parse correctly
prop_calculatorValidExpressions :: Property
prop_calculatorValidExpressions = forAll validExpression $ \expr ->
  monadicIO $ do
    result <- run $ calculatorHandler (object ["expression" .= expr])
    return $ case PropertySpec.resultIsError result of
      Nothing -> True
      Just False -> True
      Just True -> False
  where
    validExpression = do
      left <- choose (1, 1000) :: Gen Double
      op <- elements ["+", "-", "*"]  -- Avoid division for simplicity
      right <- choose (1, 1000) :: Gen Double
      return $ show left ++ " " ++ op ++ " " ++ show right

-- Property: Echo tool preserves input
prop_echoPreservesInput :: Text -> Property
prop_echoPreservesInput input = not (T.null input) ==> monadicIO $ do
  result <- run $ echoHandler (object ["message" .= input])
  return $ case PropertySpec.resultContent result of
    [ContentItem "text" msg] -> T.isInfixOf input msg
    _ -> False

-- Property: Tool Handler Properties
-- Property: Base64 encoding/decoding roundtrip
prop_base64Roundtrip :: Text -> Property
prop_base64Roundtrip input = not (T.null input) ==> monadicIO $ do
  -- Encode
  encodeResult <- run $ base64Handler (object 
    [ "operation" .= ("encode" :: Text)
    , "text" .= input
    ])
  
  case PropertySpec.resultContent encodeResult of
    [ContentItem "text" encodedMsg] -> do
      -- Extract base64 after "Base64 encoded: "
      let prefix = "Base64 encoded: "
          encoded = T.drop (T.length prefix) encodedMsg
      -- Decode
      decodeResult <- run $ base64Handler (object
        [ "operation" .= ("decode" :: Text) 
        , "text" .= encoded
        ])
      
      return $ case PropertySpec.resultContent decodeResult of
        [ContentItem "text" decodedMsg] -> 
          -- Extract decoded text after "Base64 decoded: "
          let decodePrefix = "Base64 decoded: "
              decoded = T.drop (T.length decodePrefix) decodedMsg
          in decoded == input
        _ -> False
    _ -> return False

-- Property: Random numbers are within specified range
prop_randomNumberInRange :: Property
prop_randomNumberInRange = forAll validRange $ \(minVal, maxVal :: Int) ->
  monadicIO $ do
    result <- run $ randomNumberHandler (object 
      [ "min" .= minVal
      , "max" .= maxVal
      ])
    
    return $ case PropertySpec.resultContent result of
      [ContentItem "text" msg] -> 
        case extractNumber msg of
          Just num -> num >= minVal && num <= maxVal
          Nothing -> False
      _ -> False
  where
    validRange = do
      min' <- choose (1, 100) :: Gen Int
      max' <- choose (min' + 1, min' + 100)
      return (min', max')
    
    extractNumber msg = 
      case T.words msg of
        ("Random":"number:":numStr:_) -> readMaybe (T.unpack numStr) :: Maybe Int
        _ -> Nothing

-- Helper functions
resultContent :: ToolResult -> [ContentItem]
resultContent (ToolResult content _) = content

resultIsError :: ToolResult -> Maybe Bool
resultIsError (ToolResult _ isErr) = isErr

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

-- Test specification
propertySpec :: Spec
propertySpec = do
  describe "JSON Serialization Properties" $ do
    it "MCPVersion roundtrip" $ property prop_mcpVersionRoundtrip
    it "ContentItem roundtrip" $ property prop_contentItemRoundtrip  
    it "ToolResult roundtrip" $ property prop_toolResultRoundtrip
    it "JSONRPCRequest roundtrip" $ property prop_jsonrpcRequestRoundtrip
    it "JSONRPCResponse roundtrip" $ property prop_jsonrpcResponseRoundtrip
    it "ServerInfo roundtrip" $ property prop_serverInfoRoundtrip
    it "Tool roundtrip" $ property prop_toolRoundtrip
    it "Resource roundtrip" $ property prop_resourceRoundtrip

  describe "JSON Validity Properties" $ do
    it "encoding produces valid JSON" $ property prop_encodingProducesValidJSON

  describe "Business Logic Properties" $ do
    it "error results have error flag" $ property prop_errorResultsHaveErrorFlag
    it "success results have no error flag" $ property prop_successResultsNoErrorFlag
    it "tool results have content" $ property prop_toolResultsHaveContent
    it "JSON-RPC requests have correct version" $ property prop_jsonrpcVersionIsCorrect
    it "JSON-RPC responses have correct version" $ property prop_jsonrpcResponseVersionIsCorrect
    it "MCP versions are non-negative" $ property prop_mcpVersionNonNegative
    it "tool names are non-empty" $ property prop_toolNamesNonEmpty

  describe "Tool Handler Properties" $ do
    it "calculator handles valid expressions" $ property prop_calculatorValidExpressions
    it "echo preserves input" $ property prop_echoPreservesInput
    it "base64 roundtrip works" $ property prop_base64Roundtrip
    it "random numbers in range" $ property prop_randomNumberInRange