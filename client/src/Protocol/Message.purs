module Protocol.Message
  ( GroupMessage
  , mkGroupMessage
  , verifyGroupMessage
  , extractSignedEvent
  , serializeSignedEvent
  , deserializeSignedEvent
  ) where

import Prelude

import Data.Argonaut.Core (fromObject, fromString, stringify)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError, (.:))
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Domain.Event (DomainEvent, serializeDomainEvent)
import Domain.State (SignedEvent) as DS
import Domain.Types (AID(..), MemberId(..))
import Foreign.Object as Object
import FFI.TextEncoder (encodeUtf8)
import FFI.TweetNaCl as NaCl
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode as Cesr
import Keri.Cesr.Primitive (mkPrimitive)
import Keri.Event (Event(..), eventDigest)
import Keri.Event.Interaction (InteractionConfig, mkInteraction)
import Keri.Event.Serialize (serializeEvent)
import Keri.KeyState (KeyState)
import Keri.KeyState.Verify (verifySignatures)
import Keri.Kel (SignedEvent) as Kel

-- | A group message: KERI interaction event wrapping a domain event.
type GroupMessage =
  { keriEvent :: Kel.SignedEvent
  , domainEvent :: DomainEvent
  }

-- | Create a signed group message from a domain event.
-- | Wraps the canonically-serialized domain event as an anchor
-- | in a KERI interaction event, then signs with the secret key.
mkGroupMessage
  :: { prefix :: String
     , sequenceNumber :: Int
     , priorDigest :: String
     , secretKey :: NaCl.KeyPair
     , keyIndex :: Int
     }
  -> DomainEvent
  -> Either String GroupMessage
mkGroupMessage cfg domainEvent = do
  let
    canonical = serializeDomainEvent domainEvent
    anchor = fromString canonical

    ixnConfig :: InteractionConfig
    ixnConfig =
      { prefix: cfg.prefix
      , sequenceNumber: cfg.sequenceNumber
      , priorDigest: cfg.priorDigest
      , anchors: [ anchor ]
      }
    ixn = mkInteraction ixnConfig
    serialized = serializeEvent ixn
    msgBytes = encodeUtf8 serialized
    sigBytes = NaCl.sign msgBytes cfg.secretKey.secretKey
  sigPrim <- mkPrimitive Ed25519Sig sigBytes
  let
    sigCesr = Cesr.encode sigPrim
    keriEvent =
      { event: ixn
      , signatures: [ { index: cfg.keyIndex, signature: sigCesr } ]
      }
  pure { keriEvent, domainEvent }

-- | Verify a group message's KERI signature against a key state.
verifyGroupMessage :: KeyState -> GroupMessage -> Boolean
verifyGroupMessage ks msg =
  let
    serialized = serializeEvent msg.keriEvent.event
  in
    verifySignatures ks.keys ks.signingThreshold serialized msg.keriEvent.signatures

-- | Extract a Domain.State.SignedEvent from a verified group message.
extractSignedEvent :: GroupMessage -> DS.SignedEvent
extractSignedEvent msg =
  let
    prefix = case msg.keriEvent.event of
      Interaction d -> d.prefix
      Inception d -> d.prefix
      Rotation d -> d.prefix
      Receipt d -> d.prefix
    eventId = eventDigest msg.keriEvent.event
  in
    { signer: MemberId (AID prefix)
    , eventId
    , event: msg.domainEvent
    }

-- | Serialize a SignedEvent to JSON for server storage.
-- | Produces: @{ "event": {..}, "eventId": "E...", "signer": "B..." }@
serializeSignedEvent :: DS.SignedEvent -> String
serializeSignedEvent se =
  let
    signerStr = unwrap (unwrap se.signer) :: String
    eventJson = case jsonParser (serializeDomainEvent se.event) of
      Right j -> j
      Left _ -> fromString (serializeDomainEvent se.event)
  in
    stringify
      ( fromObject
          ( Object.fromFoldable
              [ Tuple "event" eventJson
              , Tuple "eventId" (fromString se.eventId)
              , Tuple "signer" (fromString signerStr)
              ]
          )
      )

-- | Deserialize a JSON payload back to a SignedEvent.
deserializeSignedEvent :: String -> Either String DS.SignedEvent
deserializeSignedEvent s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError do
    obj <- decodeJson json
    signer <- obj .: "signer"
    eventId <- obj .: "eventId"
    event <- obj .: "event"
    pure { signer, eventId, event }
