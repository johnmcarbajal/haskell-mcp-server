{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MCP.Types (
    -- * MCP Protocol Version
    MCPVersion (..),

    -- * JSON-RPC 2.0 Message Types
    JSONRPCRequest (..),
    JSONRPCResponse (..),
    JSONRPCError (..),

    -- * MCP Server Information
    ServerInfo (..),

    -- * MCP Tool Definition
    Tool (..),

    -- * MCP Resource
    Resource (..),

    -- * Tool Call Arguments
    ToolCallArgs (..),

    -- * Tool Result
    ToolResult (..),

    -- * Content Item
    ContentItem (..),

    -- * Initialize Request Parameters
    InitializeParams (..),
    ClientCapabilities (..),
    RootsCapability (..),
    SamplingCapability (..),
    ClientInfo (..),
) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | MCP Protocol Version
data MCPVersion = MCPVersion
    { versionMajor :: Int
    , versionMinor :: Int
    }
    deriving (Show, Eq, Generic)

instance ToJSON MCPVersion where
    toJSON MCPVersion{..} = 
        String . T.pack $ printf "%d-%02d-01" versionMajor versionMinor

instance FromJSON MCPVersion where
    parseJSON = withText "MCPVersion" $ \s ->
        case T.splitOn "-" s of
            [yearStr, monthStr, _] -> do
                year <- maybe (fail "Invalid year") pure $ readMaybe (T.unpack yearStr)
                month <- maybe (fail "Invalid month") pure $ readMaybe (T.unpack monthStr)
                let version = MCPVersion year month
                -- Validate the version
                if year < 0 then fail "Negative major version not allowed"
                else if month < 0 then fail "Negative minor version not allowed"
                else if year < 2000 then fail "Major version too small (minimum: 2000)"
                else if year > 2100 then fail "Major version too large (maximum: 2100)"
                else if month > 12 then fail "Invalid month value (must be 0-12)"
                else pure version
            _ -> fail "Invalid version format - expected YYYY-MM-DD"

-- | Content Item
data ContentItem = ContentItem
    { contentType :: Text
    , contentText :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON ContentItem where
    toJSON ContentItem{..} = object 
        [ "type" .= contentType
        , "text" .= contentText
        ]

instance FromJSON ContentItem where
    parseJSON = withObject "ContentItem" $ \o ->
        ContentItem <$> o .: "type" <*> o .: "text"

-- | Tool Result
data ToolResult = ToolResult
    { resultContent :: [ContentItem]
    , resultIsError :: Maybe Bool
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolResult where
    toJSON ToolResult{..} = object
        [ "content" .= resultContent
        , "isError" .= resultIsError
        ]

instance FromJSON ToolResult where
    parseJSON = withObject "ToolResult" $ \o ->
        ToolResult <$> o .: "content" <*> o .:? "isError"

-- | JSON-RPC Error
data JSONRPCError = JSONRPCError
    { errorCode :: Int
    , errorMessage :: Text
    , errorData :: Maybe Value
    }
    deriving (Show, Eq, Generic)

instance ToJSON JSONRPCError where
    toJSON JSONRPCError{..} = object
        [ "code" .= errorCode
        , "message" .= errorMessage
        , "data" .= errorData
        ]

instance FromJSON JSONRPCError where
    parseJSON = withObject "JSONRPCError" $ \o ->
        JSONRPCError <$> o .: "code" <*> o .: "message" <*> o .:? "data"

-- | JSON-RPC Request
data JSONRPCRequest = JSONRPCRequest
    { requestJsonrpc :: Text
    , requestMethod :: Text
    , requestParams :: Maybe Value
    , requestId :: Maybe Value
    }
    deriving (Show, Eq, Generic)

instance ToJSON JSONRPCRequest where
    toJSON JSONRPCRequest{..} = object
        [ "jsonrpc" .= requestJsonrpc
        , "method" .= requestMethod
        , "params" .= requestParams
        , "id" .= requestId
        ]

instance FromJSON JSONRPCRequest where
    parseJSON = withObject "JSONRPCRequest" $ \o ->
        JSONRPCRequest <$> o .: "jsonrpc" <*> o .: "method" 
                       <*> o .:? "params" <*> o .:? "id"

-- | JSON-RPC Response
data JSONRPCResponse = JSONRPCResponse
    { responseJsonrpc :: Text
    , responseResult :: Maybe Value
    , responseError :: Maybe JSONRPCError
    , responseId :: Maybe Value
    }
    deriving (Show, Eq, Generic)

instance ToJSON JSONRPCResponse where
    toJSON JSONRPCResponse{..} = object $ 
        [ "jsonrpc" .= responseJsonrpc, "id" .= responseId ] <>
        maybe [] (\r -> ["result" .= r]) responseResult <>
        maybe [] (\e -> ["error" .= e]) responseError

instance FromJSON JSONRPCResponse where
    parseJSON = withObject "JSONRPCResponse" $ \o ->
        JSONRPCResponse <$> o .: "jsonrpc" <*> o .:? "result" 
                        <*> o .:? "error" <*> o .:? "id"

-- | Server Information
data ServerInfo = ServerInfo
    { serverName :: Text
    , serverVersion :: Text
    , serverProtocolVersion :: MCPVersion
    }
    deriving (Show, Eq, Generic)

instance ToJSON ServerInfo where
    toJSON ServerInfo{..} = object
        [ "name" .= serverName
        , "version" .= serverVersion
        , "protocolVersion" .= serverProtocolVersion
        ]

instance FromJSON ServerInfo where
    parseJSON = withObject "ServerInfo" $ \o ->
        ServerInfo <$> o .: "name" <*> o .: "version" <*> o .: "protocolVersion"

-- | Tool Definition
data Tool = Tool
    { toolName :: Text
    , toolDescription :: Text
    , toolInputSchema :: Value
    }
    deriving (Show, Eq, Generic)

instance ToJSON Tool where
    toJSON Tool{..} = object
        [ "name" .= toolName
        , "description" .= toolDescription
        , "inputSchema" .= toolInputSchema
        ]

instance FromJSON Tool where
    parseJSON = withObject "Tool" $ \o ->
        Tool <$> o .: "name" <*> o .: "description" <*> o .: "inputSchema"

-- | Resource Definition
data Resource = Resource
    { resourceUri :: Text
    , resourceName :: Text
    , resourceDescription :: Maybe Text
    , resourceMimeType :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON Resource where
    toJSON Resource{..} = object
        [ "uri" .= resourceUri
        , "name" .= resourceName
        , "description" .= resourceDescription
        , "mimeType" .= resourceMimeType
        ]

instance FromJSON Resource where
    parseJSON = withObject "Resource" $ \o ->
        Resource <$> o .: "uri" <*> o .: "name" 
                 <*> o .:? "description" <*> o .:? "mimeType"

-- | Tool Call Arguments
data ToolCallArgs = ToolCallArgs
    { callName :: Text
    , callArguments :: Value
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolCallArgs where
    toJSON ToolCallArgs{..} = object
        [ "name" .= callName
        , "arguments" .= callArguments
        ]

instance FromJSON ToolCallArgs where
    parseJSON = withObject "ToolCallArgs" $ \o ->
        ToolCallArgs <$> o .: "name" <*> o .: "arguments"

-- | Client Info
data ClientInfo = ClientInfo
    { clientName :: Text
    , clientVersion :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON ClientInfo where
    toJSON ClientInfo{..} = object
        [ "name" .= clientName
        , "version" .= clientVersion
        ]

instance FromJSON ClientInfo where
    parseJSON = withObject "ClientInfo" $ \o ->
        ClientInfo <$> o .: "name" <*> o .: "version"

-- | Roots Capability
newtype RootsCapability = RootsCapability
    { rootsListChanged :: Maybe Bool
    }
    deriving (Show, Eq, Generic)

instance ToJSON RootsCapability where
    toJSON RootsCapability{..} = object ["listChanged" .= rootsListChanged]

instance FromJSON RootsCapability where
    parseJSON = withObject "RootsCapability" $ \o ->
        RootsCapability <$> o .:? "listChanged"

-- | Sampling Capability
data SamplingCapability = SamplingCapability
    deriving (Show, Eq, Generic)

instance ToJSON SamplingCapability where
    toJSON _ = object []

instance FromJSON SamplingCapability where
    parseJSON = withObject "SamplingCapability" $ const $ pure SamplingCapability

-- | Client Capabilities
data ClientCapabilities = ClientCapabilities
    { capabilitiesRoots :: Maybe RootsCapability
    , capabilitiesSampling :: Maybe SamplingCapability
    }
    deriving (Show, Eq, Generic)

instance ToJSON ClientCapabilities where
    toJSON ClientCapabilities{..} = object
        [ "roots" .= capabilitiesRoots
        , "sampling" .= capabilitiesSampling
        ]

instance FromJSON ClientCapabilities where
    parseJSON = withObject "ClientCapabilities" $ \o ->
        ClientCapabilities <$> o .:? "roots" <*> o .:? "sampling"

-- | Initialize Parameters
data InitializeParams = InitializeParams
    { initProtocolVersion :: MCPVersion
    , initCapabilities :: ClientCapabilities
    , initClientInfo :: ClientInfo
    }
    deriving (Show, Eq, Generic)

instance ToJSON InitializeParams where
    toJSON InitializeParams{..} = object
        [ "protocolVersion" .= initProtocolVersion
        , "capabilities" .= initCapabilities
        , "clientInfo" .= initClientInfo
        ]

instance FromJSON InitializeParams where
    parseJSON = withObject "InitializeParams" $ \o ->
        InitializeParams <$> o .: "protocolVersion" <*> o .: "capabilities" <*> o .: "clientInfo"