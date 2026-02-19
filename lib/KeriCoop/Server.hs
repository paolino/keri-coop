-- | Main Warp application combining API, WebSocket, and static files.
module KeriCoop.Server (
    Options (..),
    run,
) where

import Control.Concurrent.STM (newTVarIO)
import Data.Map.Strict qualified as Map
import KeriCoop.Server.Api (apiApp)
import KeriCoop.Server.Database (initDatabase)
import KeriCoop.Server.WebSocket (wsApp)
import Network.Wai (Request (..))
import Network.Wai.Application.Static (
    defaultFileServerSettings,
    staticApp,
 )
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)

-- | Server configuration.
data Options = Options
    { optPort :: Int
    , optDbPath :: FilePath
    , optStaticDir :: FilePath
    }

-- | Start the server.
run :: Options -> IO ()
run Options{..} = do
    db <- initDatabase optDbPath
    subs <- newTVarIO Map.empty
    let static = staticApp (defaultFileServerSettings optStaticDir)
        api = apiApp db subs
        app =
            websocketsOr
                defaultConnectionOptions
                (wsApp subs)
                ( \req respond ->
                    case pathInfo req of
                        ("api" : _) -> api req respond
                        _ -> static req respond
                )
    putStrLn $ "keri-coop server on port " <> show optPort
    Warp.run optPort app
