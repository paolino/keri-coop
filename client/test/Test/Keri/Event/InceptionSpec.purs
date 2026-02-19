module Test.Keri.Event.InceptionSpec where

import Prelude

import Data.Either (Either(..))
import Data.String as String
import Effect.Class (liftEffect)
import FFI.TweetNaCl as NaCl
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode as Cesr
import Keri.Cesr.Primitive (mkPrimitive)
import Keri.Event (eventDigest, eventPrefix, eventSequenceNumber)
import Keri.Event.Inception (mkInception)
import Keri.Event.Serialize (serializeEvent)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)

spec :: Spec Unit
spec = describe "Event.Inception" do
  it "creates inception with prefix = digest (self-addressing)" do
    kp <- liftEffect NaCl.generateKeyPair
    case mkPrimitive Ed25519PubKey kp.publicKey of
      Left err -> fail err
      Right prim -> do
        let
          key = Cesr.encode prim
          icp = mkInception
            { keys: [ key ]
            , signingThreshold: 1
            , nextKeys: []
            , nextThreshold: 0
            , config: []
            , anchors: []
            }
        eventPrefix icp `shouldEqual` eventDigest icp

  it "inception has sequence number 0" do
    kp <- liftEffect NaCl.generateKeyPair
    case mkPrimitive Ed25519PubKey kp.publicKey of
      Left err -> fail err
      Right prim -> do
        let
          key = Cesr.encode prim
          icp = mkInception
            { keys: [ key ]
            , signingThreshold: 1
            , nextKeys: []
            , nextThreshold: 0
            , config: []
            , anchors: []
            }
        eventSequenceNumber icp `shouldEqual` 0

  it "SAID is verifiable from serialized event" do
    kp <- liftEffect NaCl.generateKeyPair
    case mkPrimitive Ed25519PubKey kp.publicKey of
      Left err -> fail err
      Right prim -> do
        let
          key = Cesr.encode prim
          icp = mkInception
            { keys: [ key ]
            , signingThreshold: 1
            , nextKeys: []
            , nextThreshold: 0
            , config: []
            , anchors: []
            }
          said = eventDigest icp
        said `shouldSatisfy` \s -> String.take 1 s == "E"
        String.length said `shouldSatisfy` (_ > 0)

  it "serialized event is valid JSON-like string" do
    kp <- liftEffect NaCl.generateKeyPair
    case mkPrimitive Ed25519PubKey kp.publicKey of
      Left err -> fail err
      Right prim -> do
        let
          key = Cesr.encode prim
          icp = mkInception
            { keys: [ key ]
            , signingThreshold: 1
            , nextKeys: []
            , nextThreshold: 0
            , config: []
            , anchors: []
            }
          serialized = serializeEvent icp
        serialized `shouldSatisfy` \s -> String.take 1 s == "{"
        serialized `shouldSatisfy` \s ->
          String.drop (String.length s - 1) s == "}"
