{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MCP.VersionSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T

import MCP.Types

spec :: Spec
spec = describe "Protocol Version Handling" $ do
    describe "MCPVersion serialization" $ do
        it "serializes standard version correctly" $ do
            toJSON (MCPVersion 2024 11) `shouldBe` String "2024-11-01"
            
        it "serializes version with single-digit month with zero padding" $ do
            toJSON (MCPVersion 2024 5) `shouldBe` String "2024-05-01"
            
        it "serializes version with zero month with zero padding" $ do
            toJSON (MCPVersion 2024 0) `shouldBe` String "2024-00-01"
            
        it "serializes future version correctly" $ do
            toJSON (MCPVersion 2030 12) `shouldBe` String "2030-12-01"
            
        it "serializes minimum version (0,0)" $ do
            toJSON (MCPVersion 0 0) `shouldBe` String "0-00-01"
            
        it "serializes very large version numbers" $ do
            toJSON (MCPVersion 9999 99) `shouldBe` String "9999-99-01"
            
        it "handles negative major version (edge case)" $ do
            -- This tests current behavior - negative versions serialize but may not be valid
            toJSON (MCPVersion (-1) 5) `shouldBe` String "-1-05-01"
            
        it "handles negative minor version (edge case)" $ do
            toJSON (MCPVersion 2024 (-1)) `shouldBe` String "2024--1-01"

    describe "MCPVersion deserialization" $ do
        it "deserializes standard version correctly" $ do
            let jsonValue = String "2024-11-01"
            decode (encode jsonValue) `shouldBe` Just (MCPVersion 2024 11)
            
        it "deserializes version with zero-padded month" $ do
            let jsonValue = String "2024-05-01"
            decode (encode jsonValue) `shouldBe` Just (MCPVersion 2024 5)
            
        it "deserializes version with different day component" $ do
            let jsonValue = String "2024-11-15"
            decode (encode jsonValue) `shouldBe` Just (MCPVersion 2024 11)
            
        it "fails on invalid format - missing components" $ do
            let jsonValue = String "2024-11"
            (decode (encode jsonValue) :: Maybe MCPVersion) `shouldBe` Nothing
            
        it "fails on invalid format - too many components" $ do
            let jsonValue = String "2024-11-01-extra"
            (decode (encode jsonValue) :: Maybe MCPVersion) `shouldBe` Nothing
            
        it "fails on invalid format - wrong separator" $ do
            let jsonValue = String "2024/11/01"
            (decode (encode jsonValue) :: Maybe MCPVersion) `shouldBe` Nothing
            
        it "fails on non-numeric year" $ do
            let jsonValue = String "abc-11-01"
            (decode (encode jsonValue) :: Maybe MCPVersion) `shouldBe` Nothing
            
        it "fails on non-numeric month" $ do
            let jsonValue = String "2024-abc-01"
            (decode (encode jsonValue) :: Maybe MCPVersion) `shouldBe` Nothing
            
        it "fails on empty string" $ do
            let jsonValue = String ""
            (decode (encode jsonValue) :: Maybe MCPVersion) `shouldBe` Nothing
            
        it "fails on null value" $ do
            let jsonValue = Null
            (decode (encode jsonValue) :: Maybe MCPVersion) `shouldBe` Nothing
            
        it "fails on number instead of string" $ do
            let jsonValue = Number 20241101
            (decode (encode jsonValue) :: Maybe MCPVersion) `shouldBe` Nothing
            
        it "fails on object instead of string" $ do
            let jsonValue = object ["major" .= (2024 :: Int), "minor" .= (11 :: Int)]
            (decode (encode jsonValue) :: Maybe MCPVersion) `shouldBe` Nothing

    describe "MCPVersion round-trip" $ do
        it "preserves data through serialization/deserialization" $ do
            let versions = 
                    [ MCPVersion 2024 11
                    , MCPVersion 2024 5
                    , MCPVersion 2024 0
                    , MCPVersion 0 0
                    , MCPVersion 9999 12
                    , MCPVersion 1 1
                    ]
            forM_ versions $ \ver -> do
                let encoded = encode ver
                    decoded = decode encoded
                decoded `shouldBe` Just ver

    describe "Version compatibility checks" $ do
        it "compares versions for equality" $ do
            MCPVersion 2024 11 `shouldBe` MCPVersion 2024 11
            MCPVersion 2024 11 `shouldNotBe` MCPVersion 2024 12
            MCPVersion 2024 11 `shouldNotBe` MCPVersion 2025 11
            
        it "can be used in version negotiation" $ do
            -- Test that we can compare versions for compatibility
            let serverVersion = MCPVersion 2024 11
                clientVersion1 = MCPVersion 2024 11  -- exact match
                clientVersion2 = MCPVersion 2024 10  -- older minor
                clientVersion3 = MCPVersion 2023 11  -- older major
                clientVersion4 = MCPVersion 2025 11  -- newer major
                
            -- For now just test equality, but this could be extended
            -- with actual compatibility logic
            (serverVersion == clientVersion1) `shouldBe` True
            (serverVersion == clientVersion2) `shouldBe` False
            (serverVersion == clientVersion3) `shouldBe` False
            (serverVersion == clientVersion4) `shouldBe` False

    describe "InitializeParams version handling" $ do
        it "parses initialize params with valid version" $ do
            let jsonValue = object
                    [ "protocolVersion" .= String "2024-11-01"
                    , "capabilities" .= object []
                    , "clientInfo" .= object 
                        [ "name" .= String "test-client"
                        , "version" .= String "1.0.0"
                        ]
                    ]
            case parseMaybe parseJSON jsonValue of
                Just InitializeParams{..} -> do
                    initProtocolVersion `shouldBe` MCPVersion 2024 11
                Nothing -> expectationFailure "Failed to parse valid InitializeParams"
                
        it "fails to parse initialize params with invalid version format" $ do
            let jsonValue = object
                    [ "protocolVersion" .= String "invalid-version"
                    , "capabilities" .= object []
                    , "clientInfo" .= object 
                        [ "name" .= String "test-client"
                        , "version" .= String "1.0.0"
                        ]
                    ]
            (parseMaybe parseJSON jsonValue :: Maybe InitializeParams) `shouldBe` Nothing
            
        it "fails to parse initialize params with missing version" $ do
            let jsonValue = object
                    [ "capabilities" .= object []
                    , "clientInfo" .= object 
                        [ "name" .= String "test-client"
                        , "version" .= String "1.0.0"
                        ]
                    ]
            (parseMaybe parseJSON jsonValue :: Maybe InitializeParams) `shouldBe` Nothing
            
        it "fails to parse initialize params with null version" $ do
            let jsonValue = object
                    [ "protocolVersion" .= Null
                    , "capabilities" .= object []
                    , "clientInfo" .= object 
                        [ "name" .= String "test-client"
                        , "version" .= String "1.0.0"
                        ]
                    ]
            (parseMaybe parseJSON jsonValue :: Maybe InitializeParams) `shouldBe` Nothing

    describe "Edge cases and malformed input" $ do
        it "handles version with leading zeros" $ do
            let jsonValue = String "0024-01-01"
            decode (encode jsonValue) `shouldBe` Just (MCPVersion 24 1)
            
        it "handles version with Unicode characters (should fail)" $ do
            let jsonValue = String "2024-1①-01"
            (decode (encode jsonValue) :: Maybe MCPVersion) `shouldBe` Nothing
            
        it "handles version with whitespace (accepts with trimming)" $ do
            -- Note: The current implementation trims whitespace
            let jsonValue = String " 2024-11-01 "
            decode (encode jsonValue) `shouldBe` Just (MCPVersion 2024 11)
            
        it "handles version with embedded null bytes (should fail)" $ do
            let jsonValue = String "2024\0-11-01"
            (decode (encode jsonValue) :: Maybe MCPVersion) `shouldBe` Nothing
            
        it "handles extremely long version strings (overflow behavior)" $ do
            -- Note: The current implementation parses but may overflow to negative
            let longYear = T.replicate 1000 "9"
                jsonValue = String (longYear <> "-11-01")
            case decode (encode jsonValue) of
                Just (MCPVersion major minor) -> do
                    -- The parser succeeds but the integer overflows
                    major `shouldSatisfy` (< 0)  -- Overflow to negative
                    minor `shouldBe` 11
                Nothing -> expectationFailure "Expected parse to succeed with overflow"