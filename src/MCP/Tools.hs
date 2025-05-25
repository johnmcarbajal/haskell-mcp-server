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
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
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

-- | Add a tool to the server
addTool :: MCPServer -> Tool -> (Value -> IO ToolResult) -> IO ()
addTool server tool handler = atomically $ do
    modifyTVar (tools server) (tool :)
    modifyTVar (toolHandlers server) (Map.insert (getToolName tool) handler)
  where
    getToolName (Tool name _ _) = name

-- | Add a resource to the server
addResource :: MCPServer -> Resource -> IO Value -> IO ()
addResource server resource handler = atomically $ do
    modifyTVar (resources server) (resource :)
    modifyTVar (resourceHandlers server) (Map.insert (getResourceUri resource) handler)
  where
    getResourceUri (Resource uri _ _ _) = uri

-- | Example Echo Tool
echoTool :: Tool
echoTool =
    Tool
        "echo"
        "Echo back the input message"
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "message"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("Message to echo back" :: Text)
                            ]
                    ]
            , "required" .= (["message"] :: [Text])
            ]
        )

echoHandler :: Value -> IO ToolResult
echoHandler args = case fromJSON args of
    Error err ->
        return $
            ToolResult
                [ContentItem "text" ("Error parsing arguments: " <> T.pack err)]
                (Just True)
    Success obj -> case obj of
        Object o -> case KM.lookup "message" o of
            Just (String msg) ->
                return $
                    ToolResult
                        [ContentItem "text" ("Echo: " <> msg)]
                        Nothing
            _ ->
                return $
                    ToolResult
                        [ContentItem "text" "Missing or invalid 'message' parameter"]
                        (Just True)

-- | Jeff's Bacon Cheeseburger Invitation Predictor Tool
jeffInviteTool :: Tool
jeffInviteTool =
    Tool
        "jeff_invite_predictor"
        "Predict when Jeff will invite you for a bacon cheeseburger (random workday)"
        ( object
            [ "type" .= ("object" :: Text)
            , "properties" .= object []
            ]
        )

jeffInviteHandler :: Value -> IO ToolResult
jeffInviteHandler _ = do
    currentTime <- getCurrentTime
    let currentDay = utctDay currentTime

    -- Generate a random number of workdays in the future (1-30 workdays)
    randomWorkdays <- randomRIO (1, 30) :: IO Int

    -- Calculate the target date by adding workdays (skipping weekends)
    let targetDate = addWorkdays currentDay randomWorkdays

    -- Generate a random time during work hours (9 AM - 5 PM)
    randomHour <- randomRIO (9, 17) :: IO Int
    randomMinute <- randomRIO (0, 59) :: IO Int

    -- Create some fun random variations for the invitation
    invitationStyle <- do
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

    -- Format the prediction
    let dayOfWeek = formatTime defaultTimeLocale "%A" targetDate
    let dateStr = formatTime defaultTimeLocale "%B %d, %Y" targetDate
    let timeStr =
            formatTime defaultTimeLocale "%l:%M %p" $
                UTCTime targetDate (secondsToDiffTime $ fromIntegral $ randomHour * 3600 + randomMinute * 60)

    let prediction =
            T.unlines
                [ "🍔 Jeff's Bacon Cheeseburger Invitation Predictor 🥓"
                , "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
                , ""
                , "📅 **Predicted Date:** " <> T.pack dayOfWeek <> ", " <> T.pack dateStr
                , "🕐 **Estimated Time:** " <> T.pack (drop 1 timeStr) -- Remove leading space
                , "💬 **Jeff will probably say:** \"" <> invitationStyle <> "\""
                , ""
                , "🎯 **Confidence Level:** "
                    <> if randomWorkdays <= 7
                        then "Very High 🔥"
                        else
                            if randomWorkdays <= 14
                                then "High 👍"
                                else
                                    if randomWorkdays <= 21
                                        then "Moderate 🤔"
                                        else "Optimistic 🌟"
                , ""
                , "📊 **Prediction Details:**"
                , "• Days until invitation: " <> T.pack (show randomWorkdays) <> " workdays"
                , "• Likelihood of extra bacon: " <> if randomWorkdays `mod` 3 == 0 then "High 🥓🥓" else "Standard 🥓"
                , "• Recommended response: \"Absolutely! 🍔\""
                , ""
                , "⚠️  Note: Predictions based on advanced bacon-cheeseburger algorithms"
                ]

    return $
        ToolResult
            [ContentItem "text" prediction]
            Nothing

-- Helper function to add workdays (skipping weekends)
addWorkdays :: Day -> Int -> Day
addWorkdays startDay 0 = startDay
addWorkdays startDay n
    | weekday == 6 = addWorkdays (addDays 2 startDay) n -- Saturday -> Monday
    | weekday == 7 = addWorkdays (addDays 1 startDay) n -- Sunday -> Monday
    | otherwise = addWorkdays (addDays 1 startDay) (n - 1)
  where
    (_, _, weekday) = toWeekDate startDay

-- | Example Time Tool
timeTool :: Tool
timeTool =
    Tool
        "current_time"
        "Get the current time"
        ( object
            [ "type" .= ("object" :: Text)
            , "properties" .= object []
            ]
        )

timeHandler :: Value -> IO ToolResult
timeHandler _ = do
    now <- getCurrentTime
    return $
        ToolResult
            [ContentItem "text" ("Current time: " <> T.pack (show now))]
            Nothing

-- | Example Calculator Tool
calculatorTool :: Tool
calculatorTool =
    Tool
        "calculate"
        "Perform basic arithmetic calculations"
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "expression"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("Mathematical expression to evaluate (e.g., '2 + 3 * 4')" :: Text)
                            ]
                    ]
            , "required" .= (["expression"] :: [Text])
            ]
        )

calculatorHandler :: Value -> IO ToolResult
calculatorHandler args = case fromJSON args of
    Error err ->
        return $
            ToolResult
                [ContentItem "text" ("Error parsing arguments: " <> T.pack err)]
                (Just True)
    Success obj -> case obj of
        Object o -> case KM.lookup "expression" o of
            Just (String expr) -> do
                case evaluateExpression (T.unpack expr) of
                    Left err ->
                        return $
                            ToolResult
                                [ContentItem "text" ("Calculation error: " <> T.pack err)]
                                (Just True)
                    Right result ->
                        return $
                            ToolResult
                                [ContentItem "text" ("Result: " <> T.pack (show result))]
                                Nothing
            _ ->
                return $
                    ToolResult
                        [ContentItem "text" "Missing or invalid 'expression' parameter"]
                        (Just True)
        _ ->
            return $
                ToolResult
                    [ContentItem "text" "Invalid arguments format"]
                    (Just True)

-- | Random Number Generator Tool
randomNumberTool :: Tool
randomNumberTool =
    Tool
        "random_number"
        "Generate a random number within a specified range"
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "min"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("Minimum value (default: 1)" :: Text)
                            ]
                    , "max"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("Maximum value (default: 100)" :: Text)
                            ]
                    ]
            ]
        )

randomNumberHandler :: Value -> IO ToolResult
randomNumberHandler args = case fromJSON args of
    Error err ->
        return $
            ToolResult
                [ContentItem "text" ("Error parsing arguments: " <> T.pack err)]
                (Just True)
    Success obj -> case obj of
        Object o -> do
            let minVal :: Int = case KM.lookup "min" o of
                    Just (Number n) -> floor n
                    _ -> 1
            let maxVal :: Int = case KM.lookup "max" o of
                    Just (Number n) -> floor n
                    _ -> 100

            if minVal > maxVal
                then
                    return $
                        ToolResult
                            [ContentItem "text" "Error: minimum value must be less than or equal to maximum value"]
                            (Just True)
                else do
                    randomNum <- randomRIO (minVal, maxVal) :: IO Int
                    return $
                        ToolResult
                            [ContentItem "text" ("Random number: " <> T.pack (show randomNum) <> " (range: " <> T.pack (show minVal) <> "-" <> T.pack (show maxVal) <> ")")]
                            Nothing
        _ ->
            return $
                ToolResult
                    [ContentItem "text" "Invalid arguments format"]
                    (Just True)

-- | Text Analysis Tool
textAnalysisTool :: Tool
textAnalysisTool =
    Tool
        "text_analysis"
        "Analyze text and provide statistics (word count, character count, etc.)"
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "text"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("Text to analyze" :: Text)
                            ]
                    ]
            , "required" .= (["text"] :: [Text])
            ]
        )

textAnalysisHandler :: Value -> IO ToolResult
textAnalysisHandler args = case fromJSON args of
    Error err ->
        return $
            ToolResult
                [ContentItem "text" ("Error parsing arguments: " <> T.pack err)]
                (Just True)
    Success obj -> case obj of
        Object o -> case KM.lookup "text" o of
            Just (String text) -> do
                let charCount = T.length text
                let wordCount = length $ T.words text
                let lineCount = length $ T.lines text
                let sentences = length $ filter (`elem` (".!?" :: String)) $ T.unpack text
                let paragraphs = length $ filter (not . T.null) $ T.splitOn "\n\n" text

                let analysis =
                        T.unlines
                            [ "📊 Text Analysis Results:"
                            , "━━━━━━━━━━━━━━━━━━━━━━━━"
                            , "📝 Characters: " <> T.pack (show charCount)
                            , "🔤 Words: " <> T.pack (show wordCount)
                            , "📄 Lines: " <> T.pack (show lineCount)
                            , "📋 Sentences: " <> T.pack (show sentences)
                            , "📑 Paragraphs: " <> T.pack (show paragraphs)
                            , ""
                            , "📈 Averages:"
                            , "• Characters per word: " <> T.pack (show $ if wordCount > 0 then fromIntegral charCount / fromIntegral wordCount else 0 :: Double)
                            , "• Words per sentence: " <> T.pack (show $ if sentences > 0 then fromIntegral wordCount / fromIntegral sentences else 0 :: Double)
                            ]

                return $
                    ToolResult
                        [ContentItem "text" analysis]
                        Nothing
            _ ->
                return $
                    ToolResult
                        [ContentItem "text" "Missing or invalid 'text' parameter"]
                        (Just True)
        _ ->
            return $
                ToolResult
                    [ContentItem "text" "Invalid arguments format"]
                    (Just True)

-- | Mock Weather Tool (demonstrates external API pattern)
weatherTool :: Tool
weatherTool =
    Tool
        "weather"
        "Get weather information for a location (mock data for demo)"
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "location"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("Location to get weather for" :: Text)
                            ]
                    ]
            , "required" .= (["location"] :: [Text])
            ]
        )

weatherHandler :: Value -> IO ToolResult
weatherHandler args = case fromJSON args of
    Error err ->
        return $
            ToolResult
                [ContentItem "text" ("Error parsing arguments: " <> T.pack err)]
                (Just True)
    Success obj -> case obj of
        Object o -> case KM.lookup "location" o of
            Just (String location) -> do
                -- Generate some mock weather data
                temp <- randomRIO (15, 30) :: IO Int
                humidity <- randomRIO (40, 90) :: IO Int
                conditions <- do
                    r <- randomRIO (1, 4) :: IO Int
                    return $ case r of
                        1 -> "Sunny"
                        2 -> "Cloudy"
                        3 -> "Rainy"
                        _ -> "Partly Cloudy"

                let weather =
                        T.unlines
                            [ "🌤️  Weather Report for " <> location
                            , "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
                            , "🌡️  Temperature: " <> T.pack (show temp) <> "°C"
                            , "💧 Humidity: " <> T.pack (show humidity) <> "%"
                            , "☁️  Conditions: " <> conditions
                            , ""
                            , "⚠️  Note: This is mock data for demonstration"
                            ]

                return $
                    ToolResult
                        [ContentItem "text" weather]
                        Nothing
            _ ->
                return $
                    ToolResult
                        [ContentItem "text" "Missing or invalid 'location' parameter"]
                        (Just True)
        _ ->
            return $
                ToolResult
                    [ContentItem "text" "Invalid arguments format"]
                    (Just True)

-- | UUID Generator Tool
uuidTool :: Tool
uuidTool =
    Tool
        "generate_uuid"
        "Generate a random UUID (Universally Unique Identifier)"
        ( object
            [ "type" .= ("object" :: Text)
            , "properties" .= object []
            ]
        )

uuidHandler :: Value -> IO ToolResult
uuidHandler _ = do
    uuid <- nextRandom
    let uuidStr = T.pack $ toString uuid
    return $
        ToolResult
            [ContentItem "text" ("Generated UUID: " <> uuidStr)]
            Nothing

-- | Base64 Encoding/Decoding Tool
base64Tool :: Tool
base64Tool =
    Tool
        "base64"
        "Encode or decode Base64 strings"
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "operation"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "enum" .= (["encode", "decode"] :: [Text])
                            , "description" .= ("Operation to perform: 'encode' or 'decode'" :: Text)
                            ]
                    , "text"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("Text to encode/decode" :: Text)
                            ]
                    ]
            , "required" .= (["operation", "text"] :: [Text])
            ]
        )

base64Handler :: Value -> IO ToolResult
base64Handler args = case fromJSON args of
    Error err ->
        return $
            ToolResult
                [ContentItem "text" ("Error parsing arguments: " <> T.pack err)]
                (Just True)
    Success obj -> case obj of
        Object o -> do
            let operation = case KM.lookup "operation" o of
                    Just (String op) -> op
                    _ -> ""
            let text = case KM.lookup "text" o of
                    Just (String t) -> t
                    _ -> ""

            case operation of
                "encode" -> do
                    let encoded = TE.decodeUtf8 $ B64.encode $ TE.encodeUtf8 text
                    return $
                        ToolResult
                            [ContentItem "text" ("Base64 encoded: " <> encoded)]
                            Nothing
                "decode" -> do
                    case B64.decode $ TE.encodeUtf8 text of
                        Left err ->
                            return $
                                ToolResult
                                    [ContentItem "text" ("Base64 decode error: " <> T.pack err)]
                                    (Just True)
                        Right decoded -> do
                            let decodedText = TE.decodeUtf8 decoded
                            return $
                                ToolResult
                                    [ContentItem "text" ("Base64 decoded: " <> decodedText)]
                                    Nothing
                _ ->
                    return $
                        ToolResult
                            [ContentItem "text" "Invalid operation. Use 'encode' or 'decode'"]
                            (Just True)
        _ ->
            return $
                ToolResult
                    [ContentItem "text" "Invalid arguments format"]
                    (Just True)
evaluateExpression :: String -> Either String Double
evaluateExpression expr
    | "+" `elem` words expr = evalBinaryOp expr "+" (+)
    | "-" `elem` words expr = evalBinaryOp expr "-" (-)
    | "*" `elem` words expr = evalBinaryOp expr "*" (*)
    | "/" `elem` words expr = evalBinaryOp expr "/" safeDiv
    | otherwise = case readMaybe expr of
        Just n -> Right n
        Nothing -> Left "Invalid expression format"
  where
    safeDiv :: Double -> Double -> Double
    safeDiv _ 0 = error "Division by zero"
    safeDiv x y = x / y

    evalBinaryOp :: String -> String -> (Double -> Double -> Double) -> Either String Double
    evalBinaryOp str op f = case break (== op) (words str) of
        (leftParts, _ : rightParts) -> do
            left <- case readMaybe (unwords leftParts) of
                Just n -> Right n
                Nothing -> Left "Invalid left operand"
            right <- case readMaybe (unwords rightParts) of
                Just n -> Right n
                Nothing -> Left "Invalid right operand"
            Right (f left right)
        _ -> Left "Operator not found"
