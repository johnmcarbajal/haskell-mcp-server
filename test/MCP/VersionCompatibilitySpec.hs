{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MCP.VersionCompatibilitySpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import Data.List (sort)
import MCP.Types

-- | Semantic version comparison for MCPVersion
-- Returns LT if first version is older, GT if newer, EQ if same
compareVersions :: MCPVersion -> MCPVersion -> Ordering
compareVersions (MCPVersion maj1 min1) (MCPVersion maj2 min2) =
    case compare maj1 maj2 of
        EQ -> compare min1 min2
        other -> other

-- | Check if two versions are compatible
-- Rules:
-- 1. Same major version = compatible (backward compatible within major version)
-- 2. Different major versions = incompatible
-- 3. Server can handle older client versions within same major version
-- 4. Client can work with newer server versions within same major version
isCompatible :: MCPVersion -> MCPVersion -> Bool
isCompatible (MCPVersion maj1 _) (MCPVersion maj2 _) = maj1 == maj2

-- | Check if server version can handle client version
-- Server should handle same or older client versions within major version
canServerHandleClient :: MCPVersion -> MCPVersion -> Bool
canServerHandleClient server@(MCPVersion sMaj sMin) client@(MCPVersion cMaj cMin) =
    isCompatible server client && sMin >= cMin

-- | Check if client can work with server
-- Client can work with same or newer server versions within major version
canClientWorkWithServer :: MCPVersion -> MCPVersion -> Bool
canClientWorkWithServer client@(MCPVersion cMaj cMin) server@(MCPVersion sMaj sMin) =
    isCompatible client server && sMin >= cMin

-- | Get the minimum compatible version for a given version
-- (same major version, minor version 0)
getMinCompatibleVersion :: MCPVersion -> MCPVersion
getMinCompatibleVersion (MCPVersion maj _) = MCPVersion maj 0

-- | Get the latest compatible version from a list
-- Returns Nothing if no compatible version exists
getLatestCompatible :: MCPVersion -> [MCPVersion] -> Maybe MCPVersion
getLatestCompatible target versions =
    case filter (isCompatible target) versions of
        [] -> Nothing
        compatible -> Just $ maximumBy compareVersions compatible
  where
    maximumBy :: (a -> a -> Ordering) -> [a] -> a
    maximumBy _ [] = error "empty list"
    maximumBy _ [x] = x
    maximumBy cmp (x:xs) = case maximumBy cmp xs of
        maxRest -> case cmp x maxRest of
            GT -> x
            _ -> maxRest

spec :: Spec
spec = describe "Version Compatibility" $ do
    describe "compareVersions" $ do
        it "compares versions correctly" $ do
            compareVersions (MCPVersion 2024 11) (MCPVersion 2024 11) `shouldBe` EQ
            compareVersions (MCPVersion 2024 10) (MCPVersion 2024 11) `shouldBe` LT
            compareVersions (MCPVersion 2024 12) (MCPVersion 2024 11) `shouldBe` GT
            compareVersions (MCPVersion 2023 11) (MCPVersion 2024 11) `shouldBe` LT
            compareVersions (MCPVersion 2025 11) (MCPVersion 2024 11) `shouldBe` GT
            
        it "compares versions with different minor versions correctly" $ do
            compareVersions (MCPVersion 2024 0) (MCPVersion 2024 1) `shouldBe` LT
            compareVersions (MCPVersion 2024 99) (MCPVersion 2024 1) `shouldBe` GT
            
        it "prioritizes major version in comparison" $ do
            -- Even with higher minor version, older major version is less
            compareVersions (MCPVersion 2023 99) (MCPVersion 2024 1) `shouldBe` LT
            compareVersions (MCPVersion 2025 1) (MCPVersion 2024 99) `shouldBe` GT

    describe "isCompatible" $ do
        it "considers same major version as compatible" $ do
            isCompatible (MCPVersion 2024 11) (MCPVersion 2024 11) `shouldBe` True
            isCompatible (MCPVersion 2024 11) (MCPVersion 2024 12) `shouldBe` True
            isCompatible (MCPVersion 2024 12) (MCPVersion 2024 11) `shouldBe` True
            isCompatible (MCPVersion 2024 0) (MCPVersion 2024 99) `shouldBe` True
            
        it "considers different major versions as incompatible" $ do
            isCompatible (MCPVersion 2024 11) (MCPVersion 2025 11) `shouldBe` False
            isCompatible (MCPVersion 2025 11) (MCPVersion 2024 11) `shouldBe` False
            isCompatible (MCPVersion 2023 11) (MCPVersion 2024 11) `shouldBe` False
            
        it "handles edge cases" $ do
            isCompatible (MCPVersion 2000 0) (MCPVersion 2000 99) `shouldBe` True
            isCompatible (MCPVersion 2000 0) (MCPVersion 2001 0) `shouldBe` False

    describe "canServerHandleClient" $ do
        it "server handles same version client" $ do
            canServerHandleClient (MCPVersion 2024 11) (MCPVersion 2024 11) `shouldBe` True
            
        it "server handles older minor version clients" $ do
            canServerHandleClient (MCPVersion 2024 11) (MCPVersion 2024 10) `shouldBe` True
            canServerHandleClient (MCPVersion 2024 11) (MCPVersion 2024 9) `shouldBe` True
            canServerHandleClient (MCPVersion 2024 11) (MCPVersion 2024 0) `shouldBe` True
            
        it "server cannot handle newer minor version clients" $ do
            canServerHandleClient (MCPVersion 2024 11) (MCPVersion 2024 12) `shouldBe` False
            canServerHandleClient (MCPVersion 2024 11) (MCPVersion 2024 99) `shouldBe` False
            
        it "server cannot handle different major version clients" $ do
            canServerHandleClient (MCPVersion 2024 11) (MCPVersion 2025 11) `shouldBe` False
            canServerHandleClient (MCPVersion 2024 11) (MCPVersion 2023 11) `shouldBe` False
            
        it "handles version negotiation scenarios" $ do
            let serverVersion = MCPVersion 2024 11
            -- Common client scenarios
            canServerHandleClient serverVersion (MCPVersion 2024 11) `shouldBe` True  -- exact match
            canServerHandleClient serverVersion (MCPVersion 2024 10) `shouldBe` True  -- older client
            canServerHandleClient serverVersion (MCPVersion 2024 12) `shouldBe` False -- newer client
            canServerHandleClient serverVersion (MCPVersion 2023 11) `shouldBe` False -- old major

    describe "canClientWorkWithServer" $ do
        it "client works with same version server" $ do
            canClientWorkWithServer (MCPVersion 2024 11) (MCPVersion 2024 11) `shouldBe` True
            
        it "client works with newer minor version servers" $ do
            canClientWorkWithServer (MCPVersion 2024 10) (MCPVersion 2024 11) `shouldBe` True
            canClientWorkWithServer (MCPVersion 2024 10) (MCPVersion 2024 12) `shouldBe` True
            canClientWorkWithServer (MCPVersion 2024 0) (MCPVersion 2024 99) `shouldBe` True
            
        it "client cannot work with older minor version servers" $ do
            canClientWorkWithServer (MCPVersion 2024 11) (MCPVersion 2024 10) `shouldBe` False
            canClientWorkWithServer (MCPVersion 2024 11) (MCPVersion 2024 0) `shouldBe` False
            
        it "client cannot work with different major version servers" $ do
            canClientWorkWithServer (MCPVersion 2024 11) (MCPVersion 2025 11) `shouldBe` False
            canClientWorkWithServer (MCPVersion 2024 11) (MCPVersion 2023 11) `shouldBe` False

    describe "getMinCompatibleVersion" $ do
        it "returns version with same major and minor 0" $ do
            getMinCompatibleVersion (MCPVersion 2024 11) `shouldBe` MCPVersion 2024 0
            getMinCompatibleVersion (MCPVersion 2025 99) `shouldBe` MCPVersion 2025 0
            getMinCompatibleVersion (MCPVersion 2001 5) `shouldBe` MCPVersion 2001 0
            
        it "handles edge cases" $ do
            getMinCompatibleVersion (MCPVersion 2000 99) `shouldBe` MCPVersion 2000 0
            getMinCompatibleVersion (MCPVersion 2100 12) `shouldBe` MCPVersion 2100 0

    describe "getLatestCompatible" $ do
        it "finds latest compatible version from list" $ do
            let target = MCPVersion 2024 11
                versions = 
                    [ MCPVersion 2024 9
                    , MCPVersion 2024 10
                    , MCPVersion 2024 12  -- latest compatible
                    , MCPVersion 2025 1   -- incompatible
                    , MCPVersion 2023 99  -- incompatible
                    ]
            getLatestCompatible target versions `shouldBe` Just (MCPVersion 2024 12)
            
        it "returns Nothing when no compatible version exists" $ do
            let target = MCPVersion 2024 11
                versions = 
                    [ MCPVersion 2025 1
                    , MCPVersion 2023 99
                    , MCPVersion 2026 0
                    ]
            getLatestCompatible target versions `shouldBe` Nothing
            
        it "handles single compatible version" $ do
            let target = MCPVersion 2024 11
                versions = [MCPVersion 2024 5]
            getLatestCompatible target versions `shouldBe` Just (MCPVersion 2024 5)
            
        it "handles empty list" $ do
            let target = MCPVersion 2024 11
            getLatestCompatible target [] `shouldBe` Nothing

    describe "Version negotiation scenarios" $ do
        it "simulates successful version negotiation" $ do
            let serverVersion = MCPVersion 2024 11
                clientVersion = MCPVersion 2024 10
                
            -- Client and server are compatible
            isCompatible serverVersion clientVersion `shouldBe` True
            -- Server can handle this client
            canServerHandleClient serverVersion clientVersion `shouldBe` True
            -- Client can work with this server
            canClientWorkWithServer clientVersion serverVersion `shouldBe` True
            
        it "simulates failed negotiation due to major version mismatch" $ do
            let serverVersion = MCPVersion 2024 11
                clientVersion = MCPVersion 2023 11
                
            isCompatible serverVersion clientVersion `shouldBe` False
            canServerHandleClient serverVersion clientVersion `shouldBe` False
            canClientWorkWithServer clientVersion serverVersion `shouldBe` False
            
        it "simulates failed negotiation due to client being too new" $ do
            let serverVersion = MCPVersion 2024 11
                clientVersion = MCPVersion 2024 12
                
            isCompatible serverVersion clientVersion `shouldBe` True
            -- But server cannot handle newer client features
            canServerHandleClient serverVersion clientVersion `shouldBe` False
            
        it "handles multiple client version preferences" $ do
            let serverVersion = MCPVersion 2024 11
                clientPreferences = 
                    [ MCPVersion 2025 1   -- too new (major)
                    , MCPVersion 2024 12  -- too new (minor)
                    , MCPVersion 2024 11  -- perfect match
                    , MCPVersion 2024 10  -- acceptable
                    , MCPVersion 2023 11  -- too old (major)
                    ]
                acceptableVersions = filter (canServerHandleClient serverVersion) clientPreferences
                
            -- Should find two acceptable versions
            length acceptableVersions `shouldBe` 2
            acceptableVersions `shouldBe` [MCPVersion 2024 11, MCPVersion 2024 10]
            
            -- Best match is the highest compatible version
            head acceptableVersions `shouldBe` MCPVersion 2024 11

    describe "Compatibility matrix tests" $ do
        it "validates common version pairs" $ do
            let testCases = 
                    [ ((2024, 11), (2024, 11), True)   -- exact match
                    , ((2024, 11), (2024, 10), True)   -- server newer
                    , ((2024, 10), (2024, 11), True)   -- client newer
                    , ((2024, 11), (2025, 11), False)  -- major mismatch
                    , ((2024, 0), (2024, 99), True)    -- extreme minor diff
                    , ((2000, 0), (2000, 0), True)     -- minimum version
                    , ((2024, 11), (2023, 11), False)  -- older major
                    ]
                    
            forM_ testCases $ \((maj1, min1), (maj2, min2), expected) -> do
                let v1 = MCPVersion maj1 min1
                    v2 = MCPVersion maj2 min2
                isCompatible v1 v2 `shouldBe` expected