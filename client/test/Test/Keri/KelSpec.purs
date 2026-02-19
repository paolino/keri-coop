module Test.Keri.KelSpec where

import Prelude

import Data.Either (Either(..))
import Effect.Class (liftEffect)
import FFI.TweetNaCl as NaCl
import FFI.TextEncoder (encodeUtf8)
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode as Cesr
import Keri.Cesr.Primitive (mkPrimitive)
import Data.ArrayBuffer.Types (Uint8Array)
import Keri.Event (Event, eventDigest, eventPrefix)
import Keri.Event.Inception (mkInception)
import Keri.Event.Interaction (mkInteraction)
import Keri.Event.Serialize (serializeEvent)
import Keri.Kel (emptyKel)
import Keri.Kel.Append (append) as Kel
import Keri.Kel.Replay (replay) as Kel
import Keri.KeyState as KS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

type SignedEvent =
  { event :: Event
  , signatures :: Array { index :: Int, signature :: String }
  }

signEvent
  :: forall r
   . { secretKey :: Uint8Array | r }
  -> Event
  -> Either String SignedEvent
signEvent kp ev =
  let
    msgStr = serializeEvent ev
    msgBytes = encodeUtf8 msgStr
    sig = NaCl.sign msgBytes kp.secretKey
  in
    case mkPrimitive Ed25519Sig sig of
      Left err -> Left err
      Right sigPrim ->
        Right { event: ev, signatures: [ { index: 0, signature: Cesr.encode sigPrim } ] }

mkIcp :: String -> Event
mkIcp key =
  mkInception
    { keys: [ key ]
    , signingThreshold: 1
    , nextKeys: []
    , nextThreshold: 0
    , config: []
    , anchors: []
    }

spec :: Spec Unit
spec = describe "KEL" do
  it "appends inception to empty KEL" do
    kp <- liftEffect NaCl.generateKeyPair
    case mkPrimitive Ed25519PubKey kp.publicKey of
      Left err -> fail err
      Right prim -> do
        let
          key = Cesr.encode prim
          icp = mkIcp key
        case signEvent kp icp of
          Left err -> fail err
          Right signed -> do
            case Kel.append emptyKel signed of
              Left err -> fail ("append failed: " <> err)
              Right kel -> do
                case Kel.replay kel of
                  Left err -> fail ("replay failed: " <> err)
                  Right ks -> do
                    KS.statePrefix ks `shouldEqual` eventPrefix icp
                    KS.stateSequenceNumber ks `shouldEqual` 0

  it "appends interaction after inception" do
    kp <- liftEffect NaCl.generateKeyPair
    case mkPrimitive Ed25519PubKey kp.publicKey of
      Left err -> fail err
      Right prim -> do
        let
          key = Cesr.encode prim
          icp = mkIcp key
        case signEvent kp icp of
          Left err -> fail err
          Right signed0 -> do
            case Kel.append emptyKel signed0 of
              Left err -> fail ("inception append: " <> err)
              Right kel0 -> do
                let
                  ixn = mkInteraction
                    { prefix: eventPrefix icp
                    , sequenceNumber: 1
                    , priorDigest: eventDigest icp
                    , anchors: []
                    }
                case signEvent kp ixn of
                  Left err -> fail err
                  Right signed1 -> do
                    case Kel.append kel0 signed1 of
                      Left err -> fail ("interaction append: " <> err)
                      Right kel1 -> do
                        case Kel.replay kel1 of
                          Left err -> fail ("replay failed: " <> err)
                          Right ks ->
                            KS.stateSequenceNumber ks `shouldEqual` 1

  it "rejects event with wrong sequence number" do
    kp <- liftEffect NaCl.generateKeyPair
    case mkPrimitive Ed25519PubKey kp.publicKey of
      Left err -> fail err
      Right prim -> do
        let
          key = Cesr.encode prim
          icp = mkIcp key
        case signEvent kp icp of
          Left err -> fail err
          Right signed0 -> do
            case Kel.append emptyKel signed0 of
              Left err -> fail ("inception append: " <> err)
              Right kel0 -> do
                let
                  badIxn = mkInteraction
                    { prefix: eventPrefix icp
                    , sequenceNumber: 5
                    , priorDigest: eventDigest icp
                    , anchors: []
                    }
                case signEvent kp badIxn of
                  Left err -> fail err
                  Right signed1 ->
                    case Kel.append kel0 signed1 of
                      Left _ -> pure unit
                      Right _ -> fail "should have rejected wrong sequence"
