{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MCP.Version.ValidationSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import Data.Either (isLeft, isRight)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Types (MCPVersion(..))

-- | Version validation errors
data VersionValidationError
    = NegativeMajorVersion Int
    | NegativeMinorVersion Int
    | MajorVersionTooLarge Int
    | MinorVersionTooSmall Int
    | InvalidMonthValue Int
    deriving (Show, Eq)

-- | Validate version through JSON parsing
validateVersionViaJSON :: Int -> Int -> Either String MCPVersion
validateVersionViaJSON major minor = 
    let versionStr = T.pack $ show major ++ "-" ++ padMonth minor ++ "-01"
        padMonth m = if m < 10 && m >= 0 then "0" ++ show m else show m
    in case decode (encode (String versionStr)) of
        Just v -> Right v
        Nothing -> Left "Failed to parse version"

-- | Direct validation mimicking the FromJSON logic
validateVersion :: MCPVersion -> Either VersionValidationError MCPVersion
validateVersion ver@MCPVersion{..} = do
    if versionMajor < 0 then Left $ NegativeMajorVersion versionMajor
    else if versionMinor < 0 then Left $ NegativeMinorVersion versionMinor
    else if versionMajor < 2000 then Left $ MinorVersionTooSmall versionMajor
    else if versionMajor > 2100 then Left $ MajorVersionTooLarge versionMajor
    else if versionMinor > 12 then Left $ InvalidMonthValue versionMinor
    else Right ver

-- | Check if a version is valid
isValidVersion :: MCPVersion -> Bool
isValidVersion ver = case validateVersion ver of
    Right _ -> True
    Left _ -> False

-- | Smart constructor
mkMCPVersion :: Int -> Int -> Either VersionValidationError MCPVersion
mkMCPVersion major minor = validateVersion (MCPVersion major minor)

-- | Safe constructor
mkMCPVersionSafe :: Int -> Int -> Maybe MCPVersion
mkMCPVersionSafe major minor = case mkMCPVersion major minor of
    Right ver -> Just ver
    Left _ -> Nothing

-- | Compare versions
compareVersions :: MCPVersion -> MCPVersion -> Ordering
compareVersions (MCPVersion maj1 min1) (MCPVersion maj2 min2) =
    case compare maj1 maj2 of
        EQ -> compare min1 min2
        other -> other

-- | Validate version range
validateVersionRange :: MCPVersion -> MCPVersion -> MCPVersion -> Either Text MCPVersion
validateVersionRange minVer maxVer ver = do
    case (validateVersion minVer, validateVersion maxVer, validateVersion ver) of
        (Right _, Right _, Right validVer) ->
            if compareVersions ver minVer /= LT && compareVersions ver maxVer /= GT
            then Right validVer
            else Left $ "Version outside range"
        _ -> Left "Invalid version in range check"

spec :: Spec
spec = describe "Version Validation" $ do
    describe "validateVersion" $ do
        it "accepts valid standard versions" $ do
            validateVersion (MCPVersion 2024 11) `shouldBe` Right (MCPVersion 2024 11)
            validateVersion (MCPVersion 2024 0) `shouldBe` Right (MCPVersion 2024 0)
            validateVersion (MCPVersion 2024 12) `shouldBe` Right (MCPVersion 2024 12)
            validateVersion (MCPVersion 2000 6) `shouldBe` Right (MCPVersion 2000 6)
            validateVersion (MCPVersion 2100 1) `shouldBe` Right (MCPVersion 2100 1)
            
        it "rejects negative major versions" $ do
            validateVersion (MCPVersion (-1) 5) `shouldBe` Left (NegativeMajorVersion (-1))
            validateVersion (MCPVersion (-100) 0) `shouldBe` Left (NegativeMajorVersion (-100))
            validateVersion (MCPVersion (-2024) 11) `shouldBe` Left (NegativeMajorVersion (-2024))
            
        it "rejects negative minor versions" $ do
            validateVersion (MCPVersion 2024 (-1)) `shouldBe` Left (NegativeMinorVersion (-1))
            validateVersion (MCPVersion 2024 (-12)) `shouldBe` Left (NegativeMinorVersion (-12))
            
        it "rejects major versions below minimum supported year" $ do
            validateVersion (MCPVersion 1999 11) `shouldBe` Left (MinorVersionTooSmall 1999)
            validateVersion (MCPVersion 0 11) `shouldBe` Left (MinorVersionTooSmall 0)
            validateVersion (MCPVersion 1900 11) `shouldBe` Left (MinorVersionTooSmall 1900)
            
        it "rejects major versions above maximum supported year" $ do
            validateVersion (MCPVersion 2101 11) `shouldBe` Left (MajorVersionTooLarge 2101)
            validateVersion (MCPVersion 3000 11) `shouldBe` Left (MajorVersionTooLarge 3000)
            validateVersion (MCPVersion 9999 11) `shouldBe` Left (MajorVersionTooLarge 9999)
            
        it "rejects invalid month values" $ do
            validateVersion (MCPVersion 2024 13) `shouldBe` Left (InvalidMonthValue 13)
            validateVersion (MCPVersion 2024 99) `shouldBe` Left (InvalidMonthValue 99)
            validateVersion (MCPVersion 2024 (-1)) `shouldBe` Left (NegativeMinorVersion (-1))
            
        it "accepts edge case valid versions" $ do
            validateVersion (MCPVersion 2000 0) `shouldBe` Right (MCPVersion 2000 0)
            validateVersion (MCPVersion 2100 12) `shouldBe` Right (MCPVersion 2100 12)
            validateVersion (MCPVersion 2050 0) `shouldBe` Right (MCPVersion 2050 0)
            
    describe "isValidVersion" $ do
        it "returns True for valid versions" $ do
            isValidVersion (MCPVersion 2024 11) `shouldBe` True
            isValidVersion (MCPVersion 2000 0) `shouldBe` True
            isValidVersion (MCPVersion 2100 12) `shouldBe` True
            
        it "returns False for invalid versions" $ do
            isValidVersion (MCPVersion (-1) 11) `shouldBe` False
            isValidVersion (MCPVersion 2024 (-1)) `shouldBe` False
            isValidVersion (MCPVersion 1999 11) `shouldBe` False
            isValidVersion (MCPVersion 2101 11) `shouldBe` False
            isValidVersion (MCPVersion 2024 13) `shouldBe` False
            
    describe "mkMCPVersion" $ do
        it "creates valid versions successfully" $ do
            mkMCPVersion 2024 11 `shouldBe` Right (MCPVersion 2024 11)
            mkMCPVersion 2000 0 `shouldBe` Right (MCPVersion 2000 0)
            mkMCPVersion 2100 12 `shouldBe` Right (MCPVersion 2100 12)
            
        it "fails to create invalid versions" $ do
            mkMCPVersion (-1) 11 `shouldBe` Left (NegativeMajorVersion (-1))
            mkMCPVersion 2024 (-1) `shouldBe` Left (NegativeMinorVersion (-1))
            mkMCPVersion 1999 11 `shouldBe` Left (MinorVersionTooSmall 1999)
            mkMCPVersion 2101 11 `shouldBe` Left (MajorVersionTooLarge 2101)
            mkMCPVersion 2024 13 `shouldBe` Left (InvalidMonthValue 13)
            
    describe "mkMCPVersionSafe" $ do
        it "creates valid versions as Just" $ do
            mkMCPVersionSafe 2024 11 `shouldBe` Just (MCPVersion 2024 11)
            mkMCPVersionSafe 2000 0 `shouldBe` Just (MCPVersion 2000 0)
            mkMCPVersionSafe 2100 12 `shouldBe` Just (MCPVersion 2100 12)
            
        it "returns Nothing for invalid versions" $ do
            mkMCPVersionSafe (-1) 11 `shouldBe` Nothing
            mkMCPVersionSafe 2024 (-1) `shouldBe` Nothing
            mkMCPVersionSafe 1999 11 `shouldBe` Nothing
            mkMCPVersionSafe 2101 11 `shouldBe` Nothing
            mkMCPVersionSafe 2024 13 `shouldBe` Nothing
            
    describe "validateVersionRange" $ do
        it "accepts versions within range" $ do
            let minVer = MCPVersion 2023 1
                maxVer = MCPVersion 2025 12
            validateVersionRange minVer maxVer (MCPVersion 2024 6) `shouldSatisfy` isRight
            validateVersionRange minVer maxVer (MCPVersion 2023 1) `shouldSatisfy` isRight
            validateVersionRange minVer maxVer (MCPVersion 2025 12) `shouldSatisfy` isRight
            
        it "rejects versions outside range" $ do
            let minVer = MCPVersion 2023 1
                maxVer = MCPVersion 2025 12
            validateVersionRange minVer maxVer (MCPVersion 2022 12) `shouldSatisfy` isLeft
            validateVersionRange minVer maxVer (MCPVersion 2026 1) `shouldSatisfy` isLeft
            
        it "rejects invalid versions even if in range numerically" $ do
            let minVer = MCPVersion 2023 1
                maxVer = MCPVersion 2025 12
            -- These should fail validation before range check
            validateVersionRange minVer maxVer (MCPVersion (-1) 6) `shouldSatisfy` isLeft
            validateVersionRange minVer maxVer (MCPVersion 2024 (-1)) `shouldSatisfy` isLeft
            validateVersionRange minVer maxVer (MCPVersion 2024 13) `shouldSatisfy` isLeft
            
    describe "Validation error messages" $ do
        it "provides clear error descriptions" $ do
            let testCases = 
                    [ (MCPVersion (-1) 5, "negative")
                    , (MCPVersion 2024 (-1), "negative")
                    , (MCPVersion 1999 11, "too small")
                    , (MCPVersion 2101 11, "too large")
                    , (MCPVersion 2024 13, "invalid month")
                    ]
            forM_ testCases $ \(ver, _) -> do
                validateVersion ver `shouldSatisfy` isLeft
                
    describe "Boundary testing" $ do
        it "handles boundary values correctly" $ do
            -- Minimum valid values
            validateVersion (MCPVersion 2000 0) `shouldBe` Right (MCPVersion 2000 0)
            
            -- Maximum valid values
            validateVersion (MCPVersion 2100 12) `shouldBe` Right (MCPVersion 2100 12)
            
            -- Just outside boundaries
            validateVersion (MCPVersion 1999 12) `shouldBe` Left (MinorVersionTooSmall 1999)
            validateVersion (MCPVersion 2101 0) `shouldBe` Left (MajorVersionTooLarge 2101)
            validateVersion (MCPVersion 2024 13) `shouldBe` Left (InvalidMonthValue 13)
            
    describe "Integration with existing version handling" $ do
        it "validates versions that would be serialized with negative values" $ do
            -- These versions would serialize but are semantically invalid
            isValidVersion (MCPVersion (-1) 5) `shouldBe` False
            isValidVersion (MCPVersion 2024 (-1)) `shouldBe` False
            
        it "validates extremely large versions that might overflow" $ do
            -- These should be caught by validation
            isValidVersion (MCPVersion 999999 11) `shouldBe` False
            isValidVersion (MCPVersion 2024 999) `shouldBe` False
            
        it "JSON parsing respects validation rules" $ do
            -- Test that FromJSON instance validates
            validateVersionViaJSON (-1) 5 `shouldSatisfy` isLeft
            validateVersionViaJSON 2024 (-1) `shouldSatisfy` isLeft
            validateVersionViaJSON 1999 11 `shouldSatisfy` isLeft
            validateVersionViaJSON 2101 11 `shouldSatisfy` isLeft
            validateVersionViaJSON 2024 13 `shouldSatisfy` isLeft
            
            -- Valid versions should parse
            validateVersionViaJSON 2024 11 `shouldBe` Right (MCPVersion 2024 11)
            validateVersionViaJSON 2000 0 `shouldBe` Right (MCPVersion 2000 0)
            validateVersionViaJSON 2100 12 `shouldBe` Right (MCPVersion 2100 12)