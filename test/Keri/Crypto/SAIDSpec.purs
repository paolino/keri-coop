module Test.Keri.Crypto.SAIDSpec where

import Prelude

import Data.Either (fromRight)
import Effect.Class (liftEffect)
import Keri.Crypto.Digest (saidPlaceholder)
import Keri.Crypto.SAID (replaceDigest, verifySaid)
import Keri.Event (Event(..), eventDigest, eventPrefix)
import Keri.Event.Interaction (mkInteraction)
import Keri.Event.Rotation (mkRotation)
import Keri.KeyState.PreRotation (commitKey)
import Test.Keri.TestHelper (mkTestInception, mkTestKeyPair)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

spec :: Spec Unit
spec = describe "Crypto.SAID" do
  describe "verifySaid" do
    it "succeeds for mkInception" do
      { event } ← mkTestInception
      verifySaid event `shouldEqual` true

    it "succeeds for mkInteraction" do
      { event: icp } ← mkTestInception
      let
        ixn = mkInteraction
          { prefix: eventPrefix icp
          , sequenceNumber: 1
          , priorDigest: eventDigest icp
          , anchors: []
          }
      verifySaid ixn `shouldEqual` true

    it "succeeds for mkRotation" do
      { event: icp, nextKeyPair } ← mkTestInception
      newNext ← liftEffect mkTestKeyPair
      let
        commitment = fromRight "" (commitKey newNext.cesrPubKey)
        rot = mkRotation
          { prefix: eventPrefix icp
          , sequenceNumber: 1
          , priorDigest: eventDigest icp
          , keys: [ nextKeyPair.cesrPubKey ]
          , signingThreshold: 1
          , nextKeys: [ commitment ]
          , nextThreshold: 1
          , config: []
          , anchors: []
          }
      verifySaid rot `shouldEqual` true

    it "fails for tampered signingThreshold" do
      { event } ← mkTestInception
      case event of
        Inception d →
          verifySaid (Inception (d { signingThreshold = 99 })) `shouldEqual` false
        _ → "inception" `shouldEqual` "not inception"

    it "fails for wrong digest field" do
      { event } ← mkTestInception
      case event of
        Inception d →
          verifySaid (Inception (d { digest = "wrong" })) `shouldEqual` false
        _ → "inception" `shouldEqual` "not inception"

  describe "replaceDigest" do
    it "replaces inception digest + prefix" do
      { event } ← mkTestInception
      case replaceDigest event of
        Inception d → do
          d.digest `shouldEqual` saidPlaceholder
          d.prefix `shouldEqual` saidPlaceholder
        _ → "inception" `shouldEqual` "not inception"

    it "replaces interaction digest only" do
      { event: icp } ← mkTestInception
      let
        ixn = mkInteraction
          { prefix: eventPrefix icp
          , sequenceNumber: 1
          , priorDigest: eventDigest icp
          , anchors: []
          }
      case replaceDigest ixn of
        Interaction d → do
          d.digest `shouldEqual` saidPlaceholder
          d.prefix `shouldNotEqual` saidPlaceholder
        _ → "interaction" `shouldEqual` "not interaction"
