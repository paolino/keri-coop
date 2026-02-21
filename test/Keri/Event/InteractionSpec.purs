module Test.Keri.Event.InteractionSpec where

import Prelude

import Data.String as String
import Keri.Event (Event(..), eventDigest, eventPrefix, eventSequenceNumber)
import Keri.Event.Interaction (mkInteraction)
import Test.Keri.TestHelper (mkTestInception)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Event.Interaction" do
  describe "mkInteraction" do
    it "creates an interaction event" do
      { event: icp } ← mkTestInception
      let
        ixn = mkInteraction
          { prefix: eventPrefix icp
          , sequenceNumber: 1
          , priorDigest: eventDigest icp
          , anchors: []
          }
      case ixn of
        Interaction _ → pure unit
        _ → shouldEqual "Interaction" "other"

    it "preserves prefix" do
      { event: icp } ← mkTestInception
      let
        ixn = mkInteraction
          { prefix: eventPrefix icp
          , sequenceNumber: 1
          , priorDigest: eventDigest icp
          , anchors: []
          }
      eventPrefix ixn `shouldEqual` eventPrefix icp

    it "sets sequence number" do
      { event: icp } ← mkTestInception
      let
        ixn = mkInteraction
          { prefix: eventPrefix icp
          , sequenceNumber: 3
          , priorDigest: eventDigest icp
          , anchors: []
          }
      eventSequenceNumber ixn `shouldEqual` 3

    it "SAID starts with F" do
      { event: icp } ← mkTestInception
      let
        ixn = mkInteraction
          { prefix: eventPrefix icp
          , sequenceNumber: 1
          , priorDigest: eventDigest icp
          , anchors: []
          }
      String.take 1 (eventDigest ixn) `shouldEqual` "F"
