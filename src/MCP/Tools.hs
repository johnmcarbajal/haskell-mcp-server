{-# LANGUAGE OverloadedStrings #-}

module MCP.Tools (
    addTool,
    addResource,
    echoTool,
    echoHandler,
    timeTool,
    timeHandler,
    calculatorTool,
    calculatorHandler,
) where

import Control.Concurrent.STM
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
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
        _ ->
            return $
                ToolResult
                    [ContentItem "text" "Invalid arguments format"]
                    (Just True)

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

-- | Simple expression evaluator (very basic - only handles simple operations)
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
