module Test.Protocol.MessageSpec where

import Prelude

import Data.Either (Either(..))
import Effect.Class (liftEffect)
import FFI.TweetNaCl as NaCl
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode as Cesr
import Keri.Cesr.Primitive (mkPrimitive)
import Keri.Event (Event(..), eventDigest, eventPrefix)
import Keri.Event.Inception (mkInception)
import Keri.KeyState (initialState) as KS
import Domain.Event (DomainEvent(..))
import Domain.Types (AID(..), MemberId(..), MemberName(..), PurchaseName(..))
import Protocol.Message (extractSignedEvent, mkGroupMessage, verifyGroupMessage)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)

spec :: Spec Unit
spec = describe "Protocol.Message" do
  it "creates and verifies a signed group message" do
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
          prefix = eventPrefix icp
          digest = eventDigest icp
        case icp of
          Inception d -> do
            let ks = KS.initialState d
            case
              mkGroupMessage
                { prefix
                , sequenceNumber: 1
                , priorDigest: digest
                , secretKey: kp
                , keyIndex: 0
                }
                (VoteRegisterMember (MemberName "Alice"))
              of
              Left err -> fail ("mkGroupMessage failed: " <> err)
              Right msg ->
                verifyGroupMessage ks msg `shouldEqual` true
          _ -> fail "Expected Inception event"

  it "extractSignedEvent returns correct signer" do
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
          prefix = eventPrefix icp
          digest = eventDigest icp

        case
          mkGroupMessage
            { prefix
            , sequenceNumber: 1
            , priorDigest: digest
            , secretKey: kp
            , keyIndex: 0
            }
            (OpenPurchase (PurchaseName "Oranges"))
          of
          Left err -> fail ("mkGroupMessage failed: " <> err)
          Right msg -> do
            let signed = extractSignedEvent msg
            signed.signer `shouldEqual` MemberId (AID prefix)
            case signed.event of
              OpenPurchase _ -> pure unit
              _ -> fail "Expected OpenPurchase event"
