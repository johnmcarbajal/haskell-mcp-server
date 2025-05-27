{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Example demonstrating version negotiation in MCP server
module VersionNegotiation where

import MCP.Types
import MCP.Server
import Data.Aeson
import Control.Monad (when)
import qualified Data.Text as T

-- | Version compatibility functions (from test suite)
compareVersions :: MCPVersion -> MCPVersion -> Ordering
compareVersions (MCPVersion maj1 min1) (MCPVersion maj2 min2) =
    case compare maj1 maj2 of
        EQ -> compare min1 min2
        other -> other

isCompatible :: MCPVersion -> MCPVersion -> Bool
isCompatible (MCPVersion maj1 _) (MCPVersion maj2 _) = maj1 == maj2

canServerHandleClient :: MCPVersion -> MCPVersion -> Bool
canServerHandleClient server@(MCPVersion sMaj sMin) client@(MCPVersion cMaj cMin) =
    isCompatible server client && sMin >= cMin

-- | Enhanced initialize handler with version checking
handleInitializeWithVersionCheck :: MCPServer -> JSONRPCRequest -> IO (Maybe JSONRPCResponse)
handleInitializeWithVersionCheck server JSONRPCRequest{..} = do
    case requestId of
        Nothing -> return Nothing
        Just reqId -> case requestParams of
            Nothing -> return . Just $ errorResponse reqId invalidParams "Missing params"
            Just params -> case parseMaybe parseJSON params of
                Nothing -> return . Just $ errorResponse reqId invalidParams "Invalid params"
                Just InitializeParams{..} -> do
                    let serverVersion = getServerProtocolVersion (serverInfo server)
                    
                    -- Check version compatibility
                    if not (canServerHandleClient serverVersion initProtocolVersion)
                    then do
                        logInfo $ "Rejecting incompatible client version: " ++ show initProtocolVersion
                        return . Just $ errorResponse reqId 
                            (JSONRPCError (-32602) "Incompatible protocol version" 
                                (Just . object $ 
                                    [ "serverVersion" .= serverVersion
                                    , "clientVersion" .= initProtocolVersion
                                    , "compatible" .= False
                                    ]))
                    else do
                        logInfo $ "Accepted client version: " ++ show initProtocolVersion
                        -- Continue with normal initialization
                        return . Just $ success reqId initializeResponse

-- | Example of version negotiation with multiple client preferences
negotiateVersion :: MCPVersion -> [MCPVersion] -> Either T.Text MCPVersion
negotiateVersion serverVersion clientPreferences =
    case filter (canServerHandleClient serverVersion) clientPreferences of
        [] -> Left $ "No compatible version found. Server: " <> T.pack (show serverVersion)
        (compatible:_) -> Right compatible

-- | Example server with strict version checking
createStrictServer :: IO MCPServer
createStrictServer = do
    server <- defaultServer
    -- Override the server info with specific version requirements
    let strictInfo = ServerInfo 
            { name = "strict-mcp-server"
            , version = "1.0.0"
            , protocolVersion = MCPVersion 2024 11
            }
    return server { serverInfo = strictInfo }

-- | Example: Handling version upgrade scenarios
handleVersionUpgrade :: MCPVersion -> MCPVersion -> IO ()
handleVersionUpgrade oldVersion newVersion = do
    case compareVersions oldVersion newVersion of
        EQ -> putStrLn "Versions are identical, no upgrade needed"
        LT -> do
            putStrLn $ "Upgrading from " ++ show oldVersion ++ " to " ++ show newVersion
            if isCompatible oldVersion newVersion
            then putStrLn "✓ Compatible upgrade (same major version)"
            else putStrLn "⚠️  Breaking upgrade (different major version)"
        GT -> putStrLn "Warning: Downgrading to an older version"

-- | Example: Client version selection
selectBestClientVersion :: MCPVersion -> [MCPVersion] -> Maybe MCPVersion
selectBestClientVersion serverVersion availableVersions =
    let compatible = filter (canClientWorkWithServer' serverVersion) availableVersions
        sorted = reverse $ sortBy compareVersions compatible
    in case sorted of
        [] -> Nothing
        (best:_) -> Just best
  where
    canClientWorkWithServer' server client = 
        isCompatible client server && 
        let MCPVersion _ sMin = server
            MCPVersion _ cMin = client
        in sMin >= cMin
    sortBy cmp xs = [x | (_, x) <- sort [(cmp x y, x) | x <- xs, y <- xs, x == y]]
    sort = id -- simplified for example

-- | Example usage
main :: IO ()
main = do
    putStrLn "MCP Version Negotiation Examples\n"
    
    -- Example 1: Basic compatibility check
    let serverV = MCPVersion 2024 11
        clientV1 = MCPVersion 2024 10  -- older, compatible
        clientV2 = MCPVersion 2024 12  -- newer, incompatible for server
        clientV3 = MCPVersion 2025 11  -- different major, incompatible
    
    putStrLn "Example 1: Basic Compatibility"
    putStrLn $ "Server " ++ show serverV ++ " can handle client " ++ show clientV1 ++ ": " ++ 
        show (canServerHandleClient serverV clientV1)
    putStrLn $ "Server " ++ show serverV ++ " can handle client " ++ show clientV2 ++ ": " ++ 
        show (canServerHandleClient serverV clientV2)
    putStrLn $ "Server " ++ show serverV ++ " can handle client " ++ show clientV3 ++ ": " ++ 
        show (canServerHandleClient serverV clientV3)
    
    -- Example 2: Version negotiation
    putStrLn "\nExample 2: Version Negotiation"
    let clientPrefs = [MCPVersion 2025 1, MCPVersion 2024 12, MCPVersion 2024 11, MCPVersion 2024 9]
    case negotiateVersion serverV clientPrefs of
        Left err -> putStrLn $ "Negotiation failed: " ++ T.unpack err
        Right version -> putStrLn $ "Negotiated version: " ++ show version
    
    -- Example 3: Upgrade scenarios
    putStrLn "\nExample 3: Version Upgrades"
    handleVersionUpgrade (MCPVersion 2024 10) (MCPVersion 2024 11)  -- minor upgrade
    handleVersionUpgrade (MCPVersion 2024 11) (MCPVersion 2025 1)   -- major upgrade

-- Helper functions
getServerProtocolVersion :: ServerInfo -> MCPVersion
getServerProtocolVersion (ServerInfo _ _ pv) = pv

initializeResponse :: Value
initializeResponse = object
    [ "protocolVersion" .= MCPVersion 2024 11
    , "capabilities" .= object
        [ "tools" .= object ["listChanged" .= True]
        , "resources" .= object ["subscribe" .= False, "listChanged" .= True]
        ]
    , "serverInfo" .= object
        [ "name" .= ("strict-mcp-server" :: T.Text)
        , "version" .= ("1.0.0" :: T.Text)
        ]
    ]

logInfo :: String -> IO ()
logInfo = putStrLn . ("[INFO] " ++)

invalidParams :: JSONRPCError
invalidParams = JSONRPCError (-32602) "Invalid params" Nothing

errorResponse :: Value -> JSONRPCError -> JSONRPCResponse
errorResponse reqId err = JSONRPCResponse "2.0" Nothing (Just err) (Just reqId)

success :: Value -> Value -> JSONRPCResponse
success reqId result = JSONRPCResponse "2.0" (Just result) Nothing (Just reqId)