module Protocol.Client
  ( createGroup
  , fetchEvents
  , appendEvent
  , subscribeGroup
  , getWsUrl
  ) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import FFI.Fetch as Fetch
import FFI.WebSocket as WS

-- | Compute the WebSocket base URL from the page location.
foreign import getWsUrl :: Effect String

-- | Create a new group on the server. Returns the group ID.
createGroup :: String -> Aff String
createGroup baseUrl = do
  res <- Fetch.fetch (baseUrl <> "/api/groups")
    { method: "POST", body: "" }
  when (res.status /= 200 && res.status /= 201) $
    throwError (error $ "createGroup failed: " <> show res.status)
  r :: { groupId :: String } <- parseBody res.body
  pure r.groupId

-- | Fetch events for a group after a given sequence number.
-- | Server returns @{ "events": [...] }@ envelope.
fetchEvents :: String -> String -> Int -> Aff (Array { seq :: Int, payload :: String })
fetchEvents baseUrl groupId afterSeq = do
  res <- Fetch.fetch
    (baseUrl <> "/api/groups/" <> groupId <> "/events?after=" <> show afterSeq)
    { method: "GET", body: "" }
  when (res.status /= 200) $
    throwError (error $ "fetchEvents failed: " <> show res.status)
  r :: { events :: Array { seq :: Int, payload :: String } } <- parseBody res.body
  pure r.events

-- | Append a signed event to a group's log. Returns the assigned
-- | sequence number.
appendEvent :: String -> String -> Int -> String -> Aff Int
appendEvent baseUrl groupId seq payload = do
  let body = stringify (encodeJson { seq, payload })
  res <- Fetch.fetch (baseUrl <> "/api/groups/" <> groupId <> "/events")
    { method: "POST", body }
  when (res.status /= 200 && res.status /= 201) $
    throwError (error $ "appendEvent failed: " <> show res.status)
  r :: { seq :: Int } <- parseBody res.body
  pure r.seq

-- | Subscribe to real-time events for a group via WebSocket.
subscribeGroup
  :: String
  -> String
  -> (String -> Effect Unit)
  -> Effect WS.WebSocket
subscribeGroup wsUrl groupId onEvent = do
  ws <- WS.create wsUrl
  WS.onOpen ws do
    WS.send ws (stringify (encodeJson { group: groupId }))
  WS.onMessage ws onEvent
  pure ws

-- | Parse a JSON response body.
parseBody :: forall a. DecodeJson a => String -> Aff a
parseBody body = case parse body of
  Left err -> throwError (error err)
  Right a -> pure a
  where
  parse s = lmap show (jsonParser s) >>= lmap printJsonDecodeError <<< decodeJson
