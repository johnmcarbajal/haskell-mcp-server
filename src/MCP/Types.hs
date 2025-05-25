{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
import GHC.Generics

-- | MCP Protocol Version
data MCPVersion = MCPVersion
    { major :: Int
    , minor :: Int
    }
    deriving (Show, Eq, Generic)

instance ToJSON MCPVersion
instance FromJSON MCPVersion

-- | Content Item (defined early to avoid dependencies)
data ContentItem = ContentItem
    { contentType :: Text
    , contentText :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON ContentItem where
    toJSON (ContentItem t txt) =
        object
            [ "type" .= t
            , "text" .= txt
            ]

instance FromJSON ContentItem where
    parseJSON = withObject "ContentItem" $ \o ->
        ContentItem
            <$> o .: "type"
            <*> o .: "text"

-- | Tool Result
data ToolResult = ToolResult
    { toolResultContent :: [ContentItem]
    , toolResultIsError :: Maybe Bool
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolResult where
    toJSON (ToolResult c e) =
        object
            [ "content" .= c
            , "isError" .= e
            ]

instance FromJSON ToolResult where
    parseJSON = withObject "ToolResult" $ \o ->
        ToolResult
            <$> o .: "content"
            <*> o .:? "isError"

-- | JSON-RPC Error (defined before Response)
data JSONRPCError = JSONRPCError
    { errorCode :: Int
    , errorMessage :: Text
    , errorData :: Maybe Value
    }
    deriving (Show, Eq, Generic)

instance ToJSON JSONRPCError where
    toJSON (JSONRPCError c m d) =
        object
            [ "code" .= c
            , "message" .= m
            , "data" .= d
            ]

instance FromJSON JSONRPCError where
    parseJSON = withObject "JSONRPCError" $ \o ->
        JSONRPCError
            <$> o .: "code"
            <*> o .: "message"
            <*> o .:? "data"

-- | JSON-RPC Request
data JSONRPCRequest = JSONRPCRequest
    { requestJsonrpc :: Text
    , requestMethod :: Text
    , requestParams :: Maybe Value
    , requestId :: Maybe Value
    }
    deriving (Show, Eq, Generic)

instance ToJSON JSONRPCRequest where
    toJSON (JSONRPCRequest j m p i) =
        object
            [ "jsonrpc" .= j
            , "method" .= m
            , "params" .= p
            , "id" .= i
            ]

instance FromJSON JSONRPCRequest where
    parseJSON = withObject "JSONRPCRequest" $ \o ->
        JSONRPCRequest
            <$> o .: "jsonrpc"
            <*> o .: "method"
            <*> o .:? "params"
            <*> o .:? "id"

-- | JSON-RPC Response (depends on JSONRPCError)
data JSONRPCResponse = JSONRPCResponse
    { responseJsonrpc :: Text
    , responseResult :: Maybe Value
    , responseError :: Maybe JSONRPCError
    , responseId :: Maybe Value
    }
    deriving (Show, Eq, Generic)

instance ToJSON JSONRPCResponse where
    toJSON (JSONRPCResponse j r e i) =
        object
            [ "jsonrpc" .= j
            , "result" .= r
            , "error" .= e
            , "id" .= i
            ]

instance FromJSON JSONRPCResponse where
    parseJSON = withObject "JSONRPCResponse" $ \o ->
        JSONRPCResponse
            <$> o .: "jsonrpc"
            <*> o .:? "result"
            <*> o .:? "error"
            <*> o .:? "id"

-- | MCP Server Information
data ServerInfo = ServerInfo
    { serverName :: Text
    , serverVersion :: Text
    , serverProtocolVersion :: MCPVersion
    }
    deriving (Show, Eq, Generic)

instance ToJSON ServerInfo where
    toJSON (ServerInfo n v p) =
        object
            [ "name" .= n
            , "version" .= v
            , "protocolVersion" .= p
            ]

instance FromJSON ServerInfo where
    parseJSON = withObject "ServerInfo" $ \o ->
        ServerInfo
            <$> o .: "name"
            <*> o .: "version"
            <*> o .: "protocolVersion"

-- | MCP Tool Definition
data Tool = Tool
    { toolName :: Text
    , toolDescription :: Text
    , toolInputSchema :: Value
    }
    deriving (Show, Eq, Generic)

instance ToJSON Tool where
    toJSON (Tool n d s) =
        object
            [ "name" .= n
            , "description" .= d
            , "inputSchema" .= s
            ]

instance FromJSON Tool where
    parseJSON = withObject "Tool" $ \o ->
        Tool
            <$> o .: "name"
            <*> o .: "description"
            <*> o .: "inputSchema"

-- | MCP Resource
data Resource = Resource
    { resourceUri :: Text
    , resourceName :: Text
    , resourceDescription :: Maybe Text
    , resourceMimeType :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON Resource where
    toJSON (Resource u n d m) =
        object
            [ "uri" .= u
            , "name" .= n
            , "description" .= d
            , "mimeType" .= m
            ]

instance FromJSON Resource where
    parseJSON = withObject "Resource" $ \o ->
        Resource
            <$> o .: "uri"
            <*> o .: "name"
            <*> o .:? "description"
            <*> o .:? "mimeType"

-- | Tool Call Arguments
data ToolCallArgs = ToolCallArgs
    { toolCallName :: Text
    , toolCallArguments :: Value
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolCallArgs where
    toJSON (ToolCallArgs n a) =
        object
            [ "name" .= n
            , "arguments" .= a
            ]

instance FromJSON ToolCallArgs where
    parseJSON = withObject "ToolCallArgs" $ \o ->
        ToolCallArgs
            <$> o .: "name"
            <*> o .: "arguments"

-- | Client Info
data ClientInfo = ClientInfo
    { clientName :: Text
    , clientVersion :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON ClientInfo where
    parseJSON = withObject "ClientInfo" $ \o ->
        ClientInfo
            <$> o .: "name"
            <*> o .: "version"

instance ToJSON ClientInfo where
    toJSON (ClientInfo n v) =
        object
            [ "name" .= n
            , "version" .= v
            ]

-- | Roots Capability
data RootsCapability = RootsCapability
    { rootsListChanged :: Maybe Bool
    }
    deriving (Show, Eq, Generic)

instance FromJSON RootsCapability where
    parseJSON = withObject "RootsCapability" $ \o ->
        RootsCapability
            <$> o .:? "listChanged"

instance ToJSON RootsCapability where
    toJSON (RootsCapability l) =
        object
            [ "listChanged" .= l
            ]

-- | Sampling Capability
data SamplingCapability = SamplingCapability
    {
    }
    -- Add sampling capabilities as needed
    deriving (Show, Eq, Generic)

instance FromJSON SamplingCapability where
    parseJSON = withObject "SamplingCapability" $ \_ -> return SamplingCapability

instance ToJSON SamplingCapability where
    toJSON SamplingCapability = object []

-- | Client Capabilities (depends on RootsCapability and SamplingCapability)
data ClientCapabilities = ClientCapabilities
    { clientRoots :: Maybe RootsCapability
    , clientSampling :: Maybe SamplingCapability
    }
    deriving (Show, Eq, Generic)

instance FromJSON ClientCapabilities where
    parseJSON = withObject "ClientCapabilities" $ \o ->
        ClientCapabilities
            <$> o .:? "roots"
            <*> o .:? "sampling"

instance ToJSON ClientCapabilities where
    toJSON (ClientCapabilities r s) =
        object
            [ "roots" .= r
            , "sampling" .= s
            ]

-- | Initialize Request Parameters (depends on ClientCapabilities and ClientInfo)
data InitializeParams = InitializeParams
    { initProtocolVersion :: MCPVersion
    , initCapabilities :: ClientCapabilities
    , initClientInfo :: ClientInfo
    }
    deriving (Show, Eq, Generic)

instance FromJSON InitializeParams where
    parseJSON = withObject "InitializeParams" $ \o ->
        InitializeParams
            <$> o .: "protocolVersion"
            <*> o .: "capabilities"
            <*> o .: "clientInfo"

instance ToJSON InitializeParams where
    toJSON (InitializeParams p c i) =
        object
            [ "protocolVersion" .= p
            , "capabilities" .= c
            , "clientInfo" .= i
            ]
