{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module MCP.Tools (
    -- * Tool Management
    addTool,
    addResource,
    
    -- * Core Tools
    echoTool, echoHandler,
    timeTool, timeHandler,
    calculatorTool, calculatorHandler,
    randomNumberTool, randomNumberHandler,
    textAnalysisTool, textAnalysisHandler,
    weatherTool, weatherHandler,
    uuidTool, uuidHandler,
    base64Tool, base64Handler,
    jeffInviteTool, jeffInviteHandler,
) where

import Control.Concurrent.STM

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import System.Random (randomRIO)
import Text.Printf (printf)
import Text.Read (readMaybe)

import MCP.Server
import MCP.Types

-- * Core Abstractions

-- | Tool result constructors
success, failure :: Text -> ToolResult
success msg = ToolResult [ContentItem "text" msg] Nothing
failure msg = ToolResult [ContentItem "text" msg] (Just True)

-- | Argument parser type for better composability
newtype ArgParser a = ArgParser { runParser :: Value -> Either Text a }

instance Functor ArgParser where
    fmap f (ArgParser p) = ArgParser (fmap f . p)

instance Applicative ArgParser where
    pure x = ArgParser (const $ Right x)
    ArgParser f <*> ArgParser x = ArgParser $ \v -> f v <*> x v

-- | Argument parsing combinators
required :: Text -> ArgParser Text
required key = ArgParser $ \case
    Object o -> case KM.lookup (fromString $ T.unpack key) o of
        Just (String s) -> Right s
        Just _ -> Left $ "Parameter '" <> key <> "' must be a string"
        Nothing -> Left $ "Missing required parameter: " <> key
    _ -> Left "Arguments must be an object"

optional :: Text -> a -> (Value -> Maybe a) -> ArgParser a
optional key def parser = ArgParser $ \case
    Object o -> Right $ maybe def id $ KM.lookup (fromString $ T.unpack key) o >>= parser
    _ -> Right def



optionalInt :: Text -> Int -> ArgParser Int
optionalInt key def = optional key def $ \case Number n -> Just (floor n); _ -> Nothing

enum :: Text -> [Text] -> ArgParser Text
enum key valid = ArgParser $ \case
    Object o -> case KM.lookup (fromString $ T.unpack key) o of
        Just (String s) | s `elem` valid -> Right s
                        | otherwise -> Left $ "Parameter '" <> key <> "' must be one of: " <> T.intercalate ", " valid
        Just _ -> Left $ "Parameter '" <> key <> "' must be a string"
        Nothing -> Left $ "Missing required parameter: " <> key
    _ -> Left "Arguments must be an object"

-- | Schema builders
schema :: [(Text, Value)] -> [Text] -> Value
schema props req = object
    [ "type" .= ("object" :: Text)
    , "properties" .= object [(fromString $ T.unpack k, v) | (k, v) <- props]
    , "required" .= req
    ]

stringProp, intProp :: Text -> Value
stringProp desc = object ["type" .= ("string" :: Text), "description" .= desc]
intProp desc = object ["type" .= ("integer" :: Text), "description" .= desc]

enumProp :: Text -> [Text] -> Value
enumProp desc opts = object 
    [ "type" .= ("string" :: Text)
    , "description" .= desc
    , "enum" .= opts
    ]

-- | Tool creation helper
mkTool :: Text -> Text -> [(Text, Value)] -> [Text] -> Tool
mkTool name desc props req = Tool name desc (schema props req)

-- | Parse arguments and handle errors uniformly
parseArgs :: ArgParser a -> (a -> IO ToolResult) -> Value -> IO ToolResult
parseArgs parser handler args = case runParser parser args of
    Left err -> pure $ failure err
    Right parsed -> handler parsed

-- * Server Management

addTool :: MCPServer -> Tool -> (Value -> IO ToolResult) -> IO ()
addTool MCPServer{..} tool@(Tool{toolName}) handler = atomically $ do
    modifyTVar tools (tool :)
    modifyTVar toolHandlers (Map.insert toolName handler)

addResource :: MCPServer -> Resource -> IO Value -> IO ()
addResource MCPServer{..} resource@(Resource{resourceUri}) handler = atomically $ do
    modifyTVar resources (resource :)
    modifyTVar resourceHandlers (Map.insert resourceUri handler)

-- * Tool Definitions

-- | Echo tool - demonstrates simple required parameter
echoTool :: Tool
echoTool = mkTool "echo" "Echo back the input message"
    [("message", stringProp "Message to echo back")] ["message"]

echoHandler :: Value -> IO ToolResult
echoHandler = parseArgs (required "message") $ \msg ->
    pure . success $ "Echo: " <> msg

-- | Time tool - no parameters
timeTool :: Tool
timeTool = mkTool "current_time" "Get the current time" [] []

timeHandler :: Value -> IO ToolResult
timeHandler _ = do
    now <- getCurrentTime
    pure . success $ "Current time: " <> T.pack (show now)

-- | Calculator tool - demonstrates error handling
calculatorTool :: Tool
calculatorTool = mkTool "calculate" "Perform basic arithmetic calculations"
    [("expression", stringProp "Mathematical expression (e.g., '2 + 3 * 4')")] ["expression"]

calculatorHandler :: Value -> IO ToolResult
calculatorHandler = parseArgs (required "expression") $ \expr ->
    pure $ case evaluateExpression (T.unpack expr) of
        Left err -> failure $ "Calculation error: " <> T.pack err
        Right result -> success $ "Result: " <> T.pack (show result)

-- | Random number tool - demonstrates optional parameters
randomNumberTool :: Tool
randomNumberTool = mkTool "random_number" "Generate a random number within a range"
    [ ("min", intProp "Minimum value (default: 1)")
    , ("max", intProp "Maximum value (default: 100)")
    ] []

randomNumberHandler :: Value -> IO ToolResult
randomNumberHandler = parseArgs parser $ \(minVal, maxVal) ->
    if minVal > maxVal
        then pure $ failure "Minimum must be ≤ maximum"
        else do
            num <- randomRIO (minVal, maxVal)
            pure . success $ T.pack $ printf "Random number: %d (range: %d-%d)" num minVal maxVal
  where
    parser = (,) <$> optionalInt "min" 1 <*> optionalInt "max" 100

-- | Text analysis tool
textAnalysisTool :: Tool
textAnalysisTool = mkTool "text_analysis" "Analyze text statistics"
    [("text", stringProp "Text to analyze")] ["text"]

textAnalysisHandler :: Value -> IO ToolResult
textAnalysisHandler = parseArgs (required "text") $ \text ->
    pure . success $ analyzeText text

analyzeText :: Text -> Text
analyzeText text = T.unlines
    [ "📊 Text Analysis Results:"
    , "━━━━━━━━━━━━━━━━━━━━━━━━"
    , T.pack $ printf "📝 Characters: %d" chars
    , T.pack $ printf "🔤 Words: %d" words'
    , T.pack $ printf "📄 Lines: %d" lines'
    , T.pack $ printf "📋 Sentences: %d" sentences
    , T.pack $ printf "📑 Paragraphs: %d" paragraphs
    , ""
    , "📈 Averages:"
    , T.pack $ printf "• Characters per word: %.1f" charsPerWord
    , T.pack $ printf "• Words per sentence: %.1f" wordsPerSentence
    ]
  where
    chars = T.length text
    words' = length $ T.words text
    lines' = length $ T.lines text
    sentences = length $ filter (`elem` (".!?" :: String)) $ T.unpack text
    paragraphs = length $ filter (not . T.null) $ T.splitOn "\n\n" text
    charsPerWord = if words' > 0 then fromIntegral chars / fromIntegral words' else 0 :: Double
    wordsPerSentence = if sentences > 0 then fromIntegral words' / fromIntegral sentences else 0 :: Double

-- | Weather tool - demonstrates mock data generation
weatherTool :: Tool
weatherTool = mkTool "weather" "Get weather information (mock data)"
    [("location", stringProp "Location to get weather for")] ["location"]

weatherHandler :: Value -> IO ToolResult
weatherHandler = parseArgs (required "location") $ \location -> do
    temp <- randomRIO (15, 30) :: IO Int
    humidity <- randomRIO (40, 90) :: IO Int
    conditions <- (["Sunny", "Cloudy", "Rainy", "Partly Cloudy"] !!) <$> randomRIO (0, 3)
    
    pure . success $ T.unlines
        [ "🌤️  Weather Report for " <> location
        , "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        , T.pack $ printf "🌡️  Temperature: %d°C" temp
        , T.pack $ printf "💧 Humidity: %d%%" humidity
        , "☁️  Conditions: " <> conditions
        , ""
        , "⚠️  Note: This is mock data for demonstration"
        ]

-- | UUID tool
uuidTool :: Tool
uuidTool = mkTool "generate_uuid" "Generate a random UUID" [] []

uuidHandler :: Value -> IO ToolResult
uuidHandler _ = do
    uuid <- nextRandom
    pure . success $ "Generated UUID: " <> T.pack (toString uuid)

-- | Base64 tool - demonstrates enum parameters
base64Tool :: Tool
base64Tool = mkTool "base64" "Encode or decode Base64 strings"
    [ ("operation", enumProp "Operation: 'encode' or 'decode'" ["encode", "decode"])
    , ("text", stringProp "Text to encode/decode")
    ] ["operation", "text"]

base64Handler :: Value -> IO ToolResult
base64Handler = parseArgs parser $ \(op, text) ->
    pure $ case op of
        "encode" -> success $ "Base64 encoded: " <> encodeBase64 text
        "decode" -> case decodeBase64 text of
            Left err -> failure $ "Decode error: " <> T.pack err
            Right decoded -> success $ "Base64 decoded: " <> decoded
        _ -> failure "Invalid operation"
  where
    parser = (,) <$> enum "operation" ["encode", "decode"] <*> required "text"
    encodeBase64 = TE.decodeUtf8 . B64.encode . TE.encodeUtf8
    decodeBase64 t = TE.decodeUtf8 <$> B64.decode (TE.encodeUtf8 t)

-- | Jeff's Bacon Cheeseburger Predictor - demonstrates complex logic
jeffInviteTool :: Tool
jeffInviteTool = mkTool "jeff_invite_predictor" 
    "Predict when Jeff will invite you for a bacon cheeseburger" [] []

jeffInviteHandler :: Value -> IO ToolResult
jeffInviteHandler _ = do
    days <- randomRIO (1, 30) :: IO Int
    hour <- randomRIO (9, 17) :: IO Int
    minute <- randomRIO (0, 59) :: IO Int
    invitation <- randomInvitation
    
    currentDay <- utctDay <$> getCurrentTime
    let targetDate = addWorkdays currentDay days
        timeStr = printf "%d:%02d %s" (if hour > 12 then hour - 12 else hour) minute (if hour >= 12 then "PM" else "AM" :: String)
        confidence | days <= 7 = "Very High 🔥"
                  | days <= 14 = "High 👍"  
                  | days <= 21 = "Moderate 🤔"
                  | otherwise = "Optimistic 🌟"
    
    pure . success $ T.unlines
        [ "🍔 Jeff's Bacon Cheeseburger Invitation Predictor 🥓"
        , "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        , ""
        , "📅 **Predicted Date:** " <> formatDay targetDate
        , "🕐 **Estimated Time:** " <> T.pack timeStr
        , "💬 **Jeff will probably say:** \"" <> invitation <> "\""
        , ""
        , "🎯 **Confidence Level:** " <> confidence
        , ""
        , "📊 **Prediction Details:**"
        , T.pack $ printf "• Days until invitation: %d workdays" days
        , "• Likelihood of extra bacon: " <> if days `mod` 3 == 0 then "High 🥓🥓" else "Standard 🥓"
        , "• Recommended response: \"Absolutely! 🍔\""
        , ""
        , "⚠️  Note: Predictions based on advanced bacon-cheeseburger algorithms"
        ]
  where
    randomInvitation = (invitations !!) <$> randomRIO (0, length invitations - 1)
    invitations = 
        [ "Hey, want to grab a bacon cheeseburger?"
        , "I'm thinking bacon cheeseburgers for lunch!"
        , "Bacon cheeseburger time?"
        , "Let's hit up that burger place!"
        , "Craving a bacon cheeseburger - you in?"
        , "Lunch? I'm buying bacon cheeseburgers!"
        , "Perfect day for bacon cheeseburgers!"
        , "How about some delicious bacon cheeseburgers?"
        ]
    formatDay day = T.pack $ formatTime defaultTimeLocale "%A, %B %d, %Y" day

-- * Utility Functions

addWorkdays :: Day -> Int -> Day
addWorkdays day 0 = day
addWorkdays day n
    | weekday >= 6 = addWorkdays (addDays (fromIntegral (8 - weekday)) day) n
    | otherwise = addWorkdays (addDays 1 day) (n - 1)
  where
    (_, _, weekday) = toWeekDate day

evaluateExpression :: String -> Either String Double
evaluateExpression expr = case words expr of
    [num] -> maybe (Left "Invalid number") Right $ readMaybe num
    [l, op, r] -> do
        left <- maybe (Left "Invalid left operand") Right $ readMaybe l
        right <- maybe (Left "Invalid right operand") Right $ readMaybe r
        applyOp op left right
    _ -> Left "Format: number operator number"
  where
    applyOp "+" l r = Right (l + r)
    applyOp "-" l r = Right (l - r)
    applyOp "*" l r = Right (l * r)
    applyOp "/" l r = if r == 0 then Left "Division by zero" else Right (l / r)
    applyOp op _ _ = Left $ "Unknown operator: " ++ op