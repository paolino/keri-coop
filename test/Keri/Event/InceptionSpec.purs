module Test.Keri.Event.InceptionSpec where

import Prelude

import Data.String as String
import Keri.Event (Event(..), eventDigest, eventPrefix, eventSequenceNumber)
import Test.Keri.TestHelper (mkTestInception)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "Event.Inception" do
  describe "mkInception" do
    it "creates an inception event" do
      { event } ← mkTestInception
      case event of
        Inception _ → pure unit
        _ → shouldEqual "Inception" "other"

    it "sets prefix equal to digest (self-addressing)" do
      { event } ← mkTestInception
      eventPrefix event `shouldEqual` eventDigest event

    it "sets sequence number to 0" do
      { event } ← mkTestInception
      eventSequenceNumber event `shouldEqual` 0

    it "SAID is not placeholder" do
      { event } ← mkTestInception
      let said = eventDigest event
      said `shouldSatisfy` \s →
        not (String.contains (String.Pattern "#") s)

    it "SAID starts with F (digest code)" do
      { event } ← mkTestInception
      String.take 1 (eventDigest event) `shouldEqual` "F"

    it "SAID has correct length (44)" do
      { event } ← mkTestInception
      String.length (eventDigest event) `shouldEqual` 44
