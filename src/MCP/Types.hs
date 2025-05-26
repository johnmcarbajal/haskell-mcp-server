-- Enables automatic JSON instance derivation
{-# LANGUAGE DeriveAnyClass #-}
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
import qualified Data.Text as T
import GHC.Generics
import Text.Read (readMaybe)

-- | MCP Protocol Version - fixed name shadowing and simplified JSON handling
data MCPVersion = MCPVersion
    { major :: Int
    , minor :: Int -- Renamed from 'min' to avoid shadowing Prelude.min
    }
    deriving (Show, Eq, Generic)

-- Custom JSON instances for MCPVersion because it has a special string format
instance ToJSON MCPVersion where
    toJSON (MCPVersion maj minorVer) =
        String $ T.pack $ show maj ++ "-" ++ zeroPad minorVer ++ "-01"
      where
        zeroPad n = if n < 10 then "0" ++ show n else show n

instance FromJSON MCPVersion where
    parseJSON (String s) = case T.splitOn "-" s of
        [yearStr, monthStr, _] ->
            case (readMaybe (T.unpack yearStr), readMaybe (T.unpack monthStr)) of
                (Just year, Just month) -> return $ MCPVersion year month
                _ -> fail "Invalid version format"
        _ -> fail "Invalid version format - expected YYYY-MM-DD"
    parseJSON (Object o) = MCPVersion <$> o .: "major" <*> o .: "minor"
    parseJSON _ = fail "Version must be string or object"

-- | Content Item - using generic JSON derivation since it has standard structure
data ContentItem = ContentItem
    { contentType :: Text
    , contentText :: Text
    }
    deriving (Show, Eq, Generic)

-- Custom JSON for ContentItem to match expected field names
instance ToJSON ContentItem where
    toJSON (ContentItem t txt) = object ["type" .= t, "text" .= txt]

instance FromJSON ContentItem where
    parseJSON = withObject "ContentItem" $ \o ->
        ContentItem <$> o .: "type" <*> o .: "text"

-- | Tool Result - simplified with better field naming patterns
data ToolResult = ToolResult
    { toolResultContent :: [ContentItem]
    , toolResultIsError :: Maybe Bool
    }
    deriving (Show, Eq, Generic)

-- Custom JSON to map to expected field names
instance ToJSON ToolResult where
    toJSON (ToolResult content isError) =
        object
            [ "content" .= content
            , "isError" .= isError
            ]

instance FromJSON ToolResult where
    parseJSON = withObject "ToolResult" $ \o ->
        ToolResult <$> o .: "content" <*> o .:? "isError"

-- | JSON-RPC Error - using more concise construction pattern
data JSONRPCError = JSONRPCError
    { errorCode :: Int
    , errorMessage :: Text
    , errorData :: Maybe Value
    }
    deriving (Show, Eq, Generic)

-- Simplified JSON instances using object construction helpers
instance ToJSON JSONRPCError where
    toJSON (JSONRPCError code message mData) =
        object
            [ "code" .= code
            , "message" .= message
            , "data" .= mData
            ]

instance FromJSON JSONRPCError where
    parseJSON = withObject "JSONRPCError" $ \o ->
        JSONRPCError <$> o .: "code" <*> o .: "message" <*> o .:? "data"

-- | JSON-RPC Request - standard structure suitable for generic derivation
data JSONRPCRequest = JSONRPCRequest
    { requestJsonrpc :: Text
    , requestMethod :: Text
    , requestParams :: Maybe Value
    , requestId :: Maybe Value
    }
    deriving (Show, Eq, Generic)

-- Custom JSON to handle field name mapping
instance ToJSON JSONRPCRequest where
    toJSON (JSONRPCRequest jsonrpc method params reqId) =
        object
            [ "jsonrpc" .= jsonrpc
            , "method" .= method
            , "params" .= params
            , "id" .= reqId
            ]

instance FromJSON JSONRPCRequest where
    parseJSON = withObject "JSONRPCRequest" $ \o ->
        JSONRPCRequest
            <$> o .: "jsonrpc"
            <*> o .: "method"
            <*> o .:? "params"
            <*> o .:? "id"

-- | JSON-RPC Response - improved conditional field inclusion
data JSONRPCResponse = JSONRPCResponse
    { responseJsonrpc :: Text
    , responseResult :: Maybe Value
    , responseError :: Maybe JSONRPCError
    , responseId :: Maybe Value
    }
    deriving (Show, Eq, Generic)

instance ToJSON JSONRPCResponse where
    toJSON (JSONRPCResponse jsonrpc mResult mError respId) =
        object $ ["jsonrpc" .= jsonrpc, "id" .= respId] ++ optionalFields
      where
        optionalFields =
            maybe [] (\result -> ["result" .= result]) mResult
                ++ maybe [] (\err -> ["error" .= err]) mError

instance FromJSON JSONRPCResponse where
    parseJSON = withObject "JSONRPCResponse" $ \o ->
        JSONRPCResponse
            <$> o .: "jsonrpc"
            <*> o .:? "result"
            <*> o .:? "error"
            <*> o .:? "id"

-- | Server Information - can use generic derivation with field name mapping
data ServerInfo = ServerInfo
    { serverName :: Text
    , serverVersion :: Text
    , serverProtocolVersion :: MCPVersion
    }
    deriving (Show, Eq, Generic)

-- Custom JSON to handle field name differences
instance ToJSON ServerInfo where
    toJSON (ServerInfo name version protocol) =
        object
            [ "name" .= name
            , "version" .= version
            , "protocolVersion" .= protocol
            ]

instance FromJSON ServerInfo where
    parseJSON = withObject "ServerInfo" $ \o ->
        ServerInfo
            <$> o .: "name"
            <*> o .: "version"
            <*> o .: "protocolVersion"

-- | Tool Definition - simplified construction
data Tool = Tool
    { toolName :: Text
    , toolDescription :: Text
    , toolInputSchema :: Value
    }
    deriving (Show, Eq, Generic)

instance ToJSON Tool where
    toJSON (Tool name desc schema) =
        object
            [ "name" .= name
            , "description" .= desc
            , "inputSchema" .= schema
            ]

instance FromJSON Tool where
    parseJSON = withObject "Tool" $ \o ->
        Tool <$> o .: "name" <*> o .: "description" <*> o .: "inputSchema"

-- | Resource Definition - handling optional fields cleanly
data Resource = Resource
    { resourceUri :: Text
    , resourceName :: Text
    , resourceDescription :: Maybe Text
    , resourceMimeType :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON Resource where
    toJSON (Resource uri name desc mimeType) =
        object
            [ "uri" .= uri
            , "name" .= name
            , "description" .= desc
            , "mimeType" .= mimeType
            ]

instance FromJSON Resource where
    parseJSON = withObject "Resource" $ \o ->
        Resource
            <$> o .: "uri"
            <*> o .: "name"
            <*> o .:? "description"
            <*> o .:? "mimeType"

-- | Tool Call Arguments - straightforward mapping
data ToolCallArgs = ToolCallArgs
    { toolCallName :: Text
    , toolCallArguments :: Value
    }
    deriving (Show, Eq, Generic)

instance ToJSON ToolCallArgs where
    toJSON (ToolCallArgs name args) =
        object
            [ "name" .= name
            , "arguments" .= args
            ]

instance FromJSON ToolCallArgs where
    parseJSON = withObject "ToolCallArgs" $ \o ->
        ToolCallArgs <$> o .: "name" <*> o .: "arguments"

-- | Simple types that can benefit from generic derivation with custom field names

-- Client Info - basic structure
data ClientInfo = ClientInfo
    { clientName :: Text
    , clientVersion :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON ClientInfo where
    toJSON (ClientInfo name version) = object ["name" .= name, "version" .= version]

instance FromJSON ClientInfo where
    parseJSON = withObject "ClientInfo" $ \o ->
        ClientInfo <$> o .: "name" <*> o .: "version"

-- | Capability types - these are good candidates for generic derivation
data RootsCapability = RootsCapability
    { rootsListChanged :: Maybe Bool
    }
    deriving (Show, Eq, Generic)

instance ToJSON RootsCapability where
    toJSON (RootsCapability listChanged) = object ["listChanged" .= listChanged]

instance FromJSON RootsCapability where
    parseJSON = withObject "RootsCapability" $ \o ->
        RootsCapability <$> o .:? "listChanged"

-- Sampling capability is empty for now, so we can use a simple approach
data SamplingCapability = SamplingCapability
    deriving (Show, Eq, Generic)

-- For empty types, we can use very simple JSON instances
instance ToJSON SamplingCapability where
    toJSON SamplingCapability = object []

instance FromJSON SamplingCapability where
    parseJSON = withObject "SamplingCapability" $ \_ -> return SamplingCapability

-- | Client capabilities combining the individual capability types
data ClientCapabilities = ClientCapabilities
    { clientRoots :: Maybe RootsCapability
    , clientSampling :: Maybe SamplingCapability
    }
    deriving (Show, Eq, Generic)

instance ToJSON ClientCapabilities where
    toJSON (ClientCapabilities roots sampling) =
        object
            [ "roots" .= roots
            , "sampling" .= sampling
            ]

instance FromJSON ClientCapabilities where
    parseJSON = withObject "ClientCapabilities" $ \o ->
        ClientCapabilities <$> o .:? "roots" <*> o .:? "sampling"

-- | Initialize parameters - the top-level request structure
data InitializeParams = InitializeParams
    { initProtocolVersion :: MCPVersion
    , initCapabilities :: ClientCapabilities
    , initClientInfo :: ClientInfo
    }
    deriving (Show, Eq, Generic)

instance ToJSON InitializeParams where
    toJSON (InitializeParams protocol capabilities clientInfo) =
        object
            [ "protocolVersion" .= protocol
            , "capabilities" .= capabilities
            , "clientInfo" .= clientInfo
            ]

instance FromJSON InitializeParams where
    parseJSON = withObject "InitializeParams" $ \o ->
        InitializeParams
            <$> o .: "protocolVersion"
            <*> o .: "capabilities"
            <*> o .: "clientInfo"
