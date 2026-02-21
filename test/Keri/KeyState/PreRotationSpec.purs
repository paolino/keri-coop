module Test.Keri.KeyState.PreRotationSpec where

import Prelude

import Data.Either (Either(..), isLeft, isRight)
import Data.String as String
import Effect.Class (liftEffect)
import Keri.KeyState.PreRotation (commitKey, verifyCommitment)
import Test.Keri.TestHelper (mkTestKeyPair)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "KeyState.PreRotation" do
  describe "commitKey" do
    it "produces a CESR-encoded commitment" do
      kp ← liftEffect mkTestKeyPair
      commitKey kp.cesrPubKey `shouldSatisfy` isRight

    it "commitment starts with F (digest code)" do
      kp ← liftEffect mkTestKeyPair
      case commitKey kp.cesrPubKey of
        Right c → String.take 1 c `shouldEqual` "F"
        Left _ → shouldEqual "Right" "Left"

    it "commitment has length 44" do
      kp ← liftEffect mkTestKeyPair
      case commitKey kp.cesrPubKey of
        Right c → String.length c `shouldEqual` 44
        Left _ → shouldEqual "Right" "Left"

    it "rejects invalid CESR input" do
      commitKey "not-valid-cesr" `shouldSatisfy` isLeft

    it "is deterministic" do
      kp ← liftEffect mkTestKeyPair
      case commitKey kp.cesrPubKey, commitKey kp.cesrPubKey of
        Right c1, Right c2 → c1 `shouldEqual` c2
        _, _ → shouldEqual "Right" "Left"

  describe "verifyCommitment" do
    it "verifies correct commitment" do
      kp ← liftEffect mkTestKeyPair
      case commitKey kp.cesrPubKey of
        Right c → verifyCommitment kp.cesrPubKey c `shouldEqual` true
        Left _ → shouldEqual "Right" "Left"

    it "rejects wrong key" do
      kp1 ← liftEffect mkTestKeyPair
      kp2 ← liftEffect mkTestKeyPair
      case commitKey kp1.cesrPubKey of
        Right c → verifyCommitment kp2.cesrPubKey c `shouldEqual` false
        Left _ → shouldEqual "Right" "Left"

    it "rejects wrong commitment" do
      kp ← liftEffect mkTestKeyPair
      verifyCommitment kp.cesrPubKey "Fwrong_commitment_value_xxxxxxxxxxxxxxxxxx" `shouldEqual` false
