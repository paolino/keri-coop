-- | HTTP API routes for the event log.
module KeriCoop.Server.Api (
    apiApp,
) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Data.Aeson (
    Value,
    decode,
    encode,
    object,
    (.:),
    (.=),
 )
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import KeriCoop.Server.Database (
    AppendError (..),
    Database,
    appendEvent,
    createGroup,
    getEvents,
 )
import KeriCoop.Server.WebSocket (Subscribers, broadcast)
import Network.HTTP.Types (
    Status,
    hContentType,
    status200,
    status201,
    status400,
    status404,
    status409,
 )
import Network.Wai (
    Application,
    Request (..),
    Response,
    getRequestBodyChunk,
    responseLBS,
 )

-- | WAI application for the HTTP API.
apiApp :: Database -> TVar Subscribers -> Application
apiApp db subs req respond =
    case (requestMethod req, pathInfo req) of
        ("POST", ["api", "groups"]) ->
            handleCreateGroup db req respond
        ("GET", ["api", "groups", gid, "events"]) ->
            handleGetEvents db gid req respond
        ("POST", ["api", "groups", gid, "events"]) ->
            handleAppendEvent db subs gid req respond
        _ -> respond $ jsonError status404 "not found"

handleCreateGroup ::
    Database -> Request -> (Response -> IO a) -> IO a
handleCreateGroup db req respond = do
    body <- consumeBody req
    case parseField "groupId" body of
        Nothing -> respond $ jsonError status400 "missing groupId"
        Just gid -> do
            createGroup db gid
            respond $ jsonOk status201 $ object ["groupId" .= gid]

handleGetEvents ::
    Database -> Text -> Request -> (Response -> IO a) -> IO a
handleGetEvents db gid req respond = do
    let afterSeq = parseAfter (queryString req)
    events <- getEvents db gid afterSeq
    respond $
        jsonOk status200 $
            object
                [ "events"
                    .= [ object ["seq" .= s, "payload" .= p]
                       | (s, p) <- events
                       ]
                ]

handleAppendEvent ::
    Database ->
    TVar Subscribers ->
    Text ->
    Request ->
    (Response -> IO a) ->
    IO a
handleAppendEvent db subs gid req respond = do
    body <- consumeBody req
    case parseSeqPayload body of
        Nothing -> respond $ jsonError status400 "missing seq or payload"
        Just (seqNo, payload) -> do
            result <- appendEvent db gid seqNo payload
            case result of
                Left SeqConflict ->
                    respond $ jsonError status409 "sequence conflict"
                Right s -> do
                    subscribers <- readTVarIO subs
                    let conns = Map.findWithDefault Map.empty gid subscribers
                    broadcast
                        conns
                        ( encode $
                            object ["seq" .= s, "payload" .= payload]
                        )
                    respond $ jsonOk status201 $ object ["seq" .= s]

-- | Read the entire request body.
consumeBody :: Request -> IO LBS.ByteString
consumeBody req = LBS.fromChunks <$> go
  where
    go :: IO [ByteString]
    go = do
        chunk <- getRequestBodyChunk req
        if BS.null chunk then pure [] else (chunk :) <$> go

parseField :: Text -> LBS.ByteString -> Maybe Text
parseField field body = do
    obj <- decode body
    parseMaybe (\o -> o .: Key.fromText field) obj

parseSeqPayload :: LBS.ByteString -> Maybe (Int64, Text)
parseSeqPayload body = do
    obj <- decode body
    parseMaybe (\o -> (,) <$> o .: "seq" <*> o .: "payload") obj

parseAfter :: [(ByteString, Maybe ByteString)] -> Int64
parseAfter qs = case lookup "after" qs of
    Just (Just v) -> case BS8.readInt v of
        Just (n, _) -> fromIntegral n
        Nothing -> 0
    _ -> 0

jsonOk :: Status -> Value -> Response
jsonOk s v =
    responseLBS s [(hContentType, "application/json")] (encode v)

jsonError :: Status -> Text -> Response
jsonError s msg = jsonOk s (object ["error" .= msg])
