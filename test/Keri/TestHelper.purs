module Test.Keri.TestHelper where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (fromRight)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import FFI.TextEncoder (encodeUtf8)
import FFI.TweetNaCl as NaCl
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode as CesrEncode
import Keri.Crypto.Ed25519 (generateKeyPair, publicKeyBytes, secretKeyBytes)
import Keri.Event (Event)
import Keri.Event.Inception (InceptionConfig, mkInception)
import Keri.Event.Serialize (serializeEvent)
import Keri.Kel (SignedEvent)
import Keri.KeyState.PreRotation (commitKey)

type TestKeyPair =
  { publicKey :: Uint8Array
  , secretKey :: Uint8Array
  , cesrPubKey :: String
  }

mkTestKeyPair :: Effect TestKeyPair
mkTestKeyPair = do
  kp ← generateKeyPair
  let cesrPubKey = CesrEncode.encode { code: Ed25519PubKey, raw: publicKeyBytes kp }
  pure { publicKey: publicKeyBytes kp, secretKey: secretKeyBytes kp, cesrPubKey }

mkTestInception
  :: ∀ m
   . MonadEffect m
  => m { keyPair :: TestKeyPair, nextKeyPair :: TestKeyPair, event :: Event }
mkTestInception = do
  kp ← liftEffect mkTestKeyPair
  nextKp ← liftEffect mkTestKeyPair
  let
    commitment = fromRight "" (commitKey nextKp.cesrPubKey)

    cfg :: InceptionConfig
    cfg =
      { keys: [ kp.cesrPubKey ]
      , signingThreshold: 1
      , nextKeys: [ commitment ]
      , nextThreshold: 1
      , config: []
      , anchors: []
      }
    event = mkInception cfg
  pure { keyPair: kp, nextKeyPair: nextKp, event }

signEvent :: TestKeyPair -> Event -> SignedEvent
signEvent kp event =
  let
    msgBytes = encodeUtf8 (serializeEvent event)
    sig = NaCl.sign msgBytes kp.secretKey
    cesrSig = CesrEncode.encode { code: Ed25519Sig, raw: sig }
  in
    { event, signatures: [ { index: 0, signature: cesrSig } ] }
