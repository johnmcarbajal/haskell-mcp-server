{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Tools (
    addTool,
    addResource,
    echoTool,
    echoHandler,
    timeTool,
    timeHandler,
    calculatorTool,
    calculatorHandler,
    -- New tools
    randomNumberTool,
    randomNumberHandler,
    textAnalysisTool,
    textAnalysisHandler,
    weatherTool,
    weatherHandler,
    uuidTool,
    uuidHandler,
    base64Tool,
    base64Handler,
    -- Jeff's Bacon Cheeseburger Invitation Predictor
    jeffInviteTool,
    jeffInviteHandler,
) where

import Control.Concurrent.STM
import Control.Monad (when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.String (fromString) -- Import fromString to fix the compilation error
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import System.Random (randomRIO)
import Text.Read (readMaybe)

import MCP.Server
import MCP.Types

-- | Core helper functions for reducing repetition across all handlers

-- | Create a successful tool result with text content
success :: Text -> ToolResult
success msg = ToolResult [ContentItem "text" msg] Nothing

-- | Create an error tool result
failure :: Text -> ToolResult
failure msg = ToolResult [ContentItem "text" msg] (Just True)

-- | Parse a required string parameter from JSON arguments
requireString :: Text -> Value -> Either Text Text
requireString key (Object o) = case KM.lookup (fromText key) o of
    Just (String s) -> Right s
    Just _ -> Left $ "Parameter '" <> key <> "' must be a string"
    Nothing -> Left $ "Missing required parameter: " <> key
requireString key _ = Left "Arguments must be an object"

-- | Parse an optional integer parameter with a default value
optionalInt :: Text -> Int -> Value -> Int
optionalInt key defaultVal (Object o) = case KM.lookup (fromText key) o of
    Just (Number n) -> floor n
    _ -> defaultVal
optionalInt _ defaultVal _ = defaultVal

-- | Parse an optional string parameter
optionalString :: Text -> Value -> Maybe Text
optionalString key (Object o) = case KM.lookup (fromText key) o of
    Just (String s) -> Just s
    _ -> Nothing
optionalString _ _ = Nothing

-- | Helper to convert Text to Aeson Key (for different aeson versions)
fromText :: Text -> KM.Key
fromText = fromString . T.unpack

-- | Create a JSON schema for a simple object with properties
simpleSchema :: [(Text, Value)] -> Value
simpleSchema props =
    object
        [ "type" .= ("object" :: Text)
        , "properties" .= object (map (\(k, v) -> fromText k .= v) props)
        ]

-- | Create a JSON schema with required fields
schemaWithRequired :: [(Text, Value)] -> [Text] -> Value
schemaWithRequired props required =
    object
        [ "type" .= ("object" :: Text)
        , "properties" .= object (map (\(k, v) -> fromText k .= v) props)
        , "required" .= required
        ]

-- | Standard property definitions for reuse
stringProp :: Text -> Value
stringProp desc = object ["type" .= ("string" :: Text), "description" .= desc]

intProp :: Text -> Value
intProp desc = object ["type" .= ("integer" :: Text), "description" .= desc]

enumProp :: Text -> [Text] -> Value
enumProp desc options =
    object
        [ "type" .= ("string" :: Text)
        , "description" .= desc
        , "enum" .= options
        ]

-- | Server modification functions - simplified using pattern matching
addTool :: MCPServer -> Tool -> (Value -> IO ToolResult) -> IO ()
addTool server tool@(Tool name _ _) handler = atomically $ do
    modifyTVar (tools server) (tool :)
    modifyTVar (toolHandlers server) (Map.insert name handler)

addResource :: MCPServer -> Resource -> IO Value -> IO ()
addResource server resource@(Resource uri _ _ _) handler = atomically $ do
    modifyTVar (resources server) (resource :)
    modifyTVar (resourceHandlers server) (Map.insert uri handler)

-- | Tool definitions using our helper functions
echoTool :: Tool
echoTool =
    Tool "echo" "Echo back the input message" $
        schemaWithRequired [("message", stringProp "Message to echo back")] ["message"]

echoHandler :: Value -> IO ToolResult
echoHandler args = return $ case requireString "message" args of
    Left err -> failure $ "Error: " <> err
    Right msg -> success $ "Echo: " <> msg

-- | Jeff's Bacon Cheeseburger Predictor - demonstrating complex tool with no parameters
jeffInviteTool :: Tool
jeffInviteTool =
    Tool
        "jeff_invite_predictor"
        "Predict when Jeff will invite you for a bacon cheeseburger (random workday)"
        (simpleSchema [])

jeffInviteHandler :: Value -> IO ToolResult
jeffInviteHandler _ = do
    currentTime <- getCurrentTime
    randomWorkdays <- randomRIO (1, 30) :: IO Int

    let currentDay = utctDay currentTime
        targetDate = addWorkdays currentDay randomWorkdays

    -- Generate random details for the prediction
    randomHour <- randomRIO (9, 17) :: IO Int
    randomMinute <- randomRIO (0, 59) :: IO Int
    invitationStyle <- randomInvitationMessage

    let prediction = formatJeffPrediction targetDate randomHour randomMinute invitationStyle randomWorkdays
    return $ success prediction

-- | Helper functions for Jeff's predictor - extracted for clarity
randomInvitationMessage :: IO Text
randomInvitationMessage = do
    r <- randomRIO (1, 8) :: IO Int
    return $ case r of
        1 -> "Hey, want to grab a bacon cheeseburger?"
        2 -> "I'm thinking bacon cheeseburgers for lunch!"
        3 -> "Bacon cheeseburger time?"
        4 -> "Let's hit up that burger place!"
        5 -> "Craving a bacon cheeseburger - you in?"
        6 -> "Lunch? I'm buying bacon cheeseburgers!"
        7 -> "Perfect day for bacon cheeseburgers!"
        _ -> "How about some delicious bacon cheeseburgers?"

formatJeffPrediction :: Day -> Int -> Int -> Text -> Int -> Text
formatJeffPrediction targetDate randomHour randomMinute invitation randomWorkdays =
    T.unlines
        [ "🍔 Jeff's Bacon Cheeseburger Invitation Predictor 🥓"
        , "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        , ""
        , "📅 **Predicted Date:** " <> dayOfWeek <> ", " <> dateStr
        , "🕐 **Estimated Time:** " <> timeStr
        , "💬 **Jeff will probably say:** \"" <> invitation <> "\""
        , ""
        , "🎯 **Confidence Level:** " <> confidenceLevel
        , ""
        , "📊 **Prediction Details:**"
        , "• Days until invitation: " <> T.pack (show randomWorkdays) <> " workdays"
        , "• Likelihood of extra bacon: " <> if randomWorkdays `mod` 3 == 0 then "High 🥓🥓" else "Standard 🥓"
        , "• Recommended response: \"Absolutely! 🍔\""
        , ""
        , "⚠️  Note: Predictions based on advanced bacon-cheeseburger algorithms"
        ]
  where
    dayOfWeek = T.pack $ formatTime defaultTimeLocale "%A" targetDate
    dateStr = T.pack $ formatTime defaultTimeLocale "%B %d, %Y" targetDate
    timeStr =
        T.pack $
            drop 1 $
                formatTime defaultTimeLocale "%l:%M %p" $
                    UTCTime targetDate (secondsToDiffTime $ fromIntegral $ randomHour * 3600 + randomMinute * 60)
    confidenceLevel
        | randomWorkdays <= 7 = "Very High 🔥"
        | randomWorkdays <= 14 = "High 👍"
        | randomWorkdays <= 21 = "Moderate 🤔"
        | otherwise = "Optimistic 🌟"

-- | Add workdays helper - more concise implementation with proper type handling
addWorkdays :: Day -> Int -> Day
addWorkdays day 0 = day
addWorkdays day n
    | weekday >= 6 = addWorkdays (addDays (fromIntegral (8 - weekday)) day) n -- Skip to Monday
    | otherwise = addWorkdays (addDays 1 day) (n - 1)
  where
    (_, _, weekday) = toWeekDate day

-- | Simple tools with no parameters
timeTool :: Tool
timeTool = Tool "current_time" "Get the current time" (simpleSchema [])

timeHandler :: Value -> IO ToolResult
timeHandler _ = do
    now <- getCurrentTime
    return $ success $ "Current time: " <> T.pack (show now)

uuidTool :: Tool
uuidTool = Tool "generate_uuid" "Generate a random UUID (Universally Unique Identifier)" (simpleSchema [])

uuidHandler :: Value -> IO ToolResult
uuidHandler _ = do
    uuid <- nextRandom
    return $ success $ "Generated UUID: " <> T.pack (toString uuid)

-- | Tools with single required parameter
calculatorTool :: Tool
calculatorTool =
    Tool "calculate" "Perform basic arithmetic calculations" $
        schemaWithRequired [("expression", stringProp "Mathematical expression to evaluate (e.g., '2 + 3 * 4')")] ["expression"]

calculatorHandler :: Value -> IO ToolResult
calculatorHandler args = return $ case requireString "expression" args of
    Left err -> failure err
    Right expr -> case evaluateExpression (T.unpack expr) of
        Left err -> failure $ "Calculation error: " <> T.pack err
        Right result -> success $ "Result: " <> T.pack (show result)

textAnalysisTool :: Tool
textAnalysisTool =
    Tool "text_analysis" "Analyze text and provide statistics (word count, character count, etc.)" $
        schemaWithRequired [("text", stringProp "Text to analyze")] ["text"]

textAnalysisHandler :: Value -> IO ToolResult
textAnalysisHandler args = return $ case requireString "text" args of
    Left err -> failure err
    Right text -> success $ analyzeText text

-- | Extract text analysis logic into a pure function
analyzeText :: Text -> Text
analyzeText text =
    T.unlines
        [ "📊 Text Analysis Results:"
        , "━━━━━━━━━━━━━━━━━━━━━━━━"
        , "📝 Characters: " <> showT charCount
        , "🔤 Words: " <> showT wordCount
        , "📄 Lines: " <> showT lineCount
        , "📋 Sentences: " <> showT sentences
        , "📑 Paragraphs: " <> showT paragraphs
        , ""
        , "📈 Averages:"
        , "• Characters per word: " <> showDouble charsPerWord
        , "• Words per sentence: " <> showDouble wordsPerSentence
        ]
  where
    charCount = T.length text
    wordCount = length $ T.words text
    lineCount = length $ T.lines text
    sentences = length $ filter (`elem` (".!?" :: String)) $ T.unpack text
    paragraphs = length $ filter (not . T.null) $ T.splitOn "\n\n" text
    charsPerWord = if wordCount > 0 then fromIntegral charCount / fromIntegral wordCount else 0 :: Double
    wordsPerSentence = if sentences > 0 then fromIntegral wordCount / fromIntegral sentences else 0 :: Double
    showT = T.pack . show
    showDouble = T.pack . show . (fromIntegral . round . (* 100)) . (/ 100) -- Round to 2 decimal places

weatherTool :: Tool
weatherTool =
    Tool "weather" "Get weather information for a location (mock data for demo)" $
        schemaWithRequired [("location", stringProp "Location to get weather for")] ["location"]

weatherHandler :: Value -> IO ToolResult
weatherHandler args = case requireString "location" args of
    Left err -> return $ failure err
    Right location -> do
        weatherData <- generateMockWeather location
        return $ success weatherData

-- | Generate mock weather data - extracted into pure function where possible
generateMockWeather :: Text -> IO Text
generateMockWeather location = do
    temp <- randomRIO (15, 30) :: IO Int
    humidity <- randomRIO (40, 90) :: IO Int
    conditions <- randomConditions

    return $
        T.unlines
            [ "🌤️  Weather Report for " <> location
            , "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            , "🌡️  Temperature: " <> showT temp <> "°C"
            , "💧 Humidity: " <> showT humidity <> "%"
            , "☁️  Conditions: " <> conditions
            , ""
            , "⚠️  Note: This is mock data for demonstration"
            ]
  where
    showT = T.pack . show
    randomConditions = do
        r <- randomRIO (1, 4) :: IO Int
        return $ case r of
            1 -> "Sunny"
            2 -> "Cloudy"
            3 -> "Rainy"
            _ -> "Partly Cloudy"

-- | Tools with optional parameters
randomNumberTool :: Tool
randomNumberTool =
    Tool "random_number" "Generate a random number within a specified range" $
        simpleSchema
            [ ("min", intProp "Minimum value (default: 1)")
            , ("max", intProp "Maximum value (default: 100)")
            ]

randomNumberHandler :: Value -> IO ToolResult
randomNumberHandler args = do
    let minVal = optionalInt "min" 1 args
        maxVal = optionalInt "max" 100 args

    if minVal > maxVal
        then return $ failure "Error: minimum value must be less than or equal to maximum value"
        else do
            randomNum <- randomRIO (minVal, maxVal) :: IO Int
            return $
                success $
                    T.unwords
                        [ "Random number:"
                        , showT randomNum
                        , "(range:"
                        , showT minVal <> "-" <> showT maxVal <> ")"
                        ]
  where
    showT = T.pack . show

-- | Tools with multiple required parameters and validation
base64Tool :: Tool
base64Tool =
    Tool "base64" "Encode or decode Base64 strings" $
        schemaWithRequired
            [ ("operation", enumProp "Operation to perform: 'encode' or 'decode'" ["encode", "decode"])
            , ("text", stringProp "Text to encode/decode")
            ]
            ["operation", "text"]

base64Handler :: Value -> IO ToolResult
base64Handler args = return $ case parseBase64Args args of
    Left err -> failure err
    Right (operation, text) -> case operation of
        "encode" -> success $ "Base64 encoded: " <> encodeBase64 text
        "decode" -> case decodeBase64 text of
            Left err -> failure $ "Base64 decode error: " <> T.pack err
            Right decoded -> success $ "Base64 decoded: " <> decoded
        _ -> failure "Invalid operation. Use 'encode' or 'decode'"
  where
    -- Extract the argument parsing logic into a separate function for clarity
    parseBase64Args argValue = do
        operation <- requireString "operation" argValue
        text <- requireString "text" argValue
        return (operation, text)

    encodeBase64 = TE.decodeUtf8 . B64.encode . TE.encodeUtf8
    decodeBase64 t = case B64.decode $ TE.encodeUtf8 t of
        Left err -> Left err
        Right decoded -> Right $ TE.decodeUtf8 decoded

-- | Improved calculator with better error handling and more operations
evaluateExpression :: String -> Either String Double
evaluateExpression = parseAndEval . words
  where
    parseAndEval tokens = case tokens of
        [num] -> maybeToEither "Invalid number" $ readMaybe num
        [left, op, right] -> do
            l <- maybeToEither "Invalid left operand" $ readMaybe left
            r <- maybeToEither "Invalid right operand" $ readMaybe right
            applyOp op l r
        _ -> Left "Expression must be in format: number operator number"

    applyOp "+" = \l r -> Right (l + r)
    applyOp "-" = \l r -> Right (l - r)
    applyOp "*" = \l r -> Right (l * r)
    applyOp "/" = \l r -> if r == 0 then Left "Division by zero" else Right (l / r)
    applyOp op = \_ _ -> Left $ "Unknown operator: " ++ op

    maybeToEither err Nothing = Left err
    maybeToEither _ (Just x) = Right x
