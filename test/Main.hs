{- | Integration tests for the keri-coop server.
Tests the full HTTP + WebSocket round-trip between two simulated clients.
-}
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (SomeException, catch)
import Data.Aeson (
    FromJSON,
    Value (..),
    decode,
    encode,
    object,
    (.:),
    (.=),
 )
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (parseEither, withObject)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import KeriCoop.Server.Api (apiApp)
import KeriCoop.Server.Database (initDatabase)
import KeriCoop.Server.WebSocket (wsApp)
import Network.HTTP.Client (
    RequestBody (..),
    defaultManagerSettings,
    httpLbs,
    method,
    newManager,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
 )
import Network.HTTP.Types (status200)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import System.Timeout (timeout)
import Test.Hspec (
    around,
    describe,
    hspec,
    it,
    shouldBe,
    shouldSatisfy,
 )

-- | Build a WAI application with in-memory SQLite.
buildApp :: IO Application
buildApp = do
    db <- initDatabase ":memory:"
    subs <- newTVarIO Map.empty
    let api = apiApp db subs
    pure $
        websocketsOr WS.defaultConnectionOptions (wsApp subs) $
            \req respond ->
                case pathInfo req of
                    ("api" : _) -> api req respond
                    _ -> respond $ responseLBS status200 [] "not found"

-- | POST JSON to a URL, return response body.
postJson :: String -> Value -> IO LBS.ByteString
postJson url body = do
    mgr <- newManager defaultManagerSettings
    req <- parseRequest url
    let req' =
            req
                { method = "POST"
                , requestBody = RequestBodyLBS (encode body)
                , requestHeaders = [("Content-Type", "application/json")]
                }
    responseBody <$> httpLbs req' mgr

-- | GET a URL, return response body.
getJson :: String -> IO LBS.ByteString
getJson url = do
    mgr <- newManager defaultManagerSettings
    req <- parseRequest url
    responseBody <$> httpLbs req mgr

-- | Decode a field from a JSON response body.
decodeField :: (FromJSON a) => Text -> LBS.ByteString -> Either String a
decodeField key bs = do
    v <- maybe (Left "JSON decode failed") Right (decode bs)
    parseEither (withObject "response" (\o -> o .: Key.fromText key)) v

main :: IO ()
main = hspec $ around (testWithApplication buildApp) $ do
    describe "Event sync" $ do
        it "stores and fetches events" $ \port -> do
            let base = "http://127.0.0.1:" <> show port

            -- Client A: create group
            res1 <-
                postJson (base <> "/api/groups") $
                    object ["groupId" .= ("g1" :: Text)]
            decodeField "groupId" res1 `shouldBe` Right ("g1" :: Text)

            -- Client A: append event (seq=0)
            let payload0 :: Text
                payload0 =
                    "{\"event\":{\"name\":\"Alice\",\"t\":\"VoteRegisterMember\"}\
                    \,\"eventId\":\"Esaid\",\"signer\":\"Bprefix\"}"
            res2 <-
                postJson (base <> "/api/groups/g1/events") $
                    object ["seq" .= (0 :: Int), "payload" .= payload0]
            decodeField "seq" res2 `shouldBe` Right (0 :: Int)

            -- Client B: fetch events (after=-1 to get all)
            res3 <- getJson (base <> "/api/groups/g1/events?after=-1")
            let events = decodeField "events" res3 :: Either String [Value]
            events `shouldSatisfy` \case
                Right [_] -> True
                _ -> False
            case events of
                Right [ev] -> do
                    parseEither
                        (withObject "ev" (\o -> o .: "seq"))
                        ev
                        `shouldBe` Right (0 :: Int)
                    parseEither
                        (withObject "ev" (\o -> o .: "payload"))
                        ev
                        `shouldBe` Right payload0
                _ -> fail "expected exactly one event"

        it "broadcasts events via WebSocket" $ \port -> do
            let base = "http://127.0.0.1:" <> show port

            -- Create group
            _ <-
                postJson (base <> "/api/groups") $
                    object ["groupId" .= ("g2" :: Text)]

            -- Client B: connect WebSocket and wait for a broadcast
            received <- newEmptyMVar
            _ <-
                forkIO $
                    WS.runClient
                        "127.0.0.1"
                        port
                        "/"
                        ( \conn -> do
                            WS.sendTextData
                                conn
                                (encode (object ["group" .= ("g2" :: Text)]))
                            msg <- WS.receiveData conn :: IO Text
                            putMVar received msg
                            -- Keep alive briefly so server doesn't log errors
                            threadDelay 1000000
                        )
                        `catch` \(_ :: SomeException) -> pure ()

            -- Give WebSocket time to connect and subscribe
            threadDelay 500000

            -- Client A: append event
            let payload1 :: Text
                payload1 =
                    "{\"event\":{\"name\":\"Bob\",\"t\":\"VoteRegisterMember\"}\
                    \,\"eventId\":\"Esaid2\",\"signer\":\"Bother\"}"
            _ <-
                postJson (base <> "/api/groups/g2/events") $
                    object ["seq" .= (0 :: Int), "payload" .= payload1]

            -- Client B: verify WebSocket received the broadcast
            result <- timeout 5000000 (takeMVar received)
            case result of
                Nothing ->
                    fail "WebSocket did not receive message within 5s"
                Just wsMsg -> do
                    let parsed =
                            decode (LBS.fromStrict (TE.encodeUtf8 wsMsg)) ::
                                Maybe Value
                    case parsed of
                        Just obj -> do
                            parseEither
                                (withObject "ws" (\o -> o .: "payload"))
                                obj
                                `shouldBe` Right payload1
                            parseEither
                                (withObject "ws" (\o -> o .: "seq"))
                                obj
                                `shouldBe` Right (0 :: Int)
                        Nothing ->
                            fail "expected JSON from WebSocket"
