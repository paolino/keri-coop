-- | WebSocket handler for real-time event push.
module KeriCoop.Server.WebSocket (
    Subscribers,
    wsApp,
    broadcast,
) where

import Control.Concurrent.STM (
    TVar,
    atomically,
    modifyTVar',
 )
import Control.Exception (finally)
import Data.Aeson (decode, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Unique (Unique, newUnique)
import Network.WebSockets qualified as WS

-- | Map from group ID to connected clients (keyed by unique ID).
type Subscribers = Map Text (Map Unique WS.Connection)

-- | WebSocket server application.
wsApp :: TVar Subscribers -> WS.ServerApp
wsApp subs pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (pure ()) $ do
        msg <- WS.receiveData conn
        case parseGroupJoin msg of
            Nothing -> WS.sendClose conn ("invalid handshake" :: Text)
            Just gid -> do
                uid <- newUnique
                addSubscriber subs gid uid conn
                finally
                    (keepAlive conn)
                    (removeSubscriber subs gid uid)

-- | Send a message to all subscribers of a group.
broadcast :: Map Unique WS.Connection -> LBS.ByteString -> IO ()
broadcast conns msg = mapM_ (`WS.sendTextData` msg) conns

-- | Parse the initial join message: @{ "group": "\<id\>" }@
parseGroupJoin :: LBS.ByteString -> Maybe Text
parseGroupJoin msg = do
    obj <- decode msg
    parseMaybe (\o -> o .: "group") obj

-- | Block until the client disconnects.
keepAlive :: WS.Connection -> IO ()
keepAlive conn = do
    _ <- WS.receiveDataMessage conn :: IO WS.DataMessage
    keepAlive conn

addSubscriber ::
    TVar Subscribers -> Text -> Unique -> WS.Connection -> IO ()
addSubscriber subs gid uid conn =
    atomically $
        modifyTVar' subs $
            Map.insertWith (<>) gid (Map.singleton uid conn)

removeSubscriber :: TVar Subscribers -> Text -> Unique -> IO ()
removeSubscriber subs gid uid =
    atomically $
        modifyTVar' subs $
            Map.adjust (Map.delete uid) gid
