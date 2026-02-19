module Protocol.Message
  ( GroupMessage
  , mkGroupMessage
  , verifyGroupMessage
  , extractSignedEvent
  ) where

import Prelude

import Data.Argonaut.Core (fromString)
import Data.Either (Either)
import Domain.Event (DomainEvent, serializeDomainEvent)
import Domain.State (SignedEvent) as DS
import Domain.Types (AID(..), MemberId(..))
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
