module Test.Keri.Event.RotationSpec where

import Prelude

import Data.Either (fromRight)
import Data.String as String
import Keri.Event (Event(..), eventDigest, eventPrefix)
import Keri.Event.Rotation (mkRotation)
import Keri.KeyState.PreRotation (commitKey)
import Test.Keri.TestHelper (mkTestInception, mkTestKeyPair)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

spec :: Spec Unit
spec = describe "Event.Rotation" do
  describe "mkRotation" do
    it "creates a rotation event" do
      { event: icp } ← mkTestInception
      newKp ← liftEffect mkTestKeyPair
      nextKp ← liftEffect mkTestKeyPair
      let
        commitment = fromRight "" (commitKey nextKp.cesrPubKey)
        rot = mkRotation
          { prefix: eventPrefix icp
          , sequenceNumber: 1
          , priorDigest: eventDigest icp
          , keys: [ newKp.cesrPubKey ]
          , signingThreshold: 1
          , nextKeys: [ commitment ]
          , nextThreshold: 1
          , config: []
          , anchors: []
          }
      case rot of
        Rotation _ → pure unit
        _ → shouldEqual "Rotation" "other"

    it "preserves prefix" do
      { event: icp } ← mkTestInception
      newKp ← liftEffect mkTestKeyPair
      nextKp ← liftEffect mkTestKeyPair
      let
        commitment = fromRight "" (commitKey nextKp.cesrPubKey)
        rot = mkRotation
          { prefix: eventPrefix icp
          , sequenceNumber: 1
          , priorDigest: eventDigest icp
          , keys: [ newKp.cesrPubKey ]
          , signingThreshold: 1
          , nextKeys: [ commitment ]
          , nextThreshold: 1
          , config: []
          , anchors: []
          }
      eventPrefix rot `shouldEqual` eventPrefix icp

    it "SAID differs from prefix" do
      { event: icp } ← mkTestInception
      newKp ← liftEffect mkTestKeyPair
      nextKp ← liftEffect mkTestKeyPair
      let
        commitment = fromRight "" (commitKey nextKp.cesrPubKey)
        rot = mkRotation
          { prefix: eventPrefix icp
          , sequenceNumber: 1
          , priorDigest: eventDigest icp
          , keys: [ newKp.cesrPubKey ]
          , signingThreshold: 1
          , nextKeys: [ commitment ]
          , nextThreshold: 1
          , config: []
          , anchors: []
          }
      eventDigest rot `shouldNotEqual` eventPrefix icp

    it "SAID starts with F" do
      { event: icp } ← mkTestInception
      newKp ← liftEffect mkTestKeyPair
      nextKp ← liftEffect mkTestKeyPair
      let
        commitment = fromRight "" (commitKey nextKp.cesrPubKey)
        rot = mkRotation
          { prefix: eventPrefix icp
          , sequenceNumber: 1
          , priorDigest: eventDigest icp
          , keys: [ newKp.cesrPubKey ]
          , signingThreshold: 1
          , nextKeys: [ commitment ]
          , nextThreshold: 1
          , config: []
          , anchors: []
          }
      String.take 1 (eventDigest rot) `shouldEqual` "F"
