module Test.Keri.Crypto.DigestSpec where

import Prelude

import Data.String as String
import Keri.Cesr.DerivationCode (totalLength, DerivationCode(..))
import Keri.Crypto.Digest (computeSaid, saidPlaceholder)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "Crypto.Digest" do
  describe "saidPlaceholder" do
    it "has correct length" do
      String.length saidPlaceholder `shouldEqual` totalLength Blake2bDigest

    it "is all hash characters" do
      saidPlaceholder `shouldSatisfy` \s →
        String.length (String.replaceAll (String.Pattern "#") (String.Replacement "") s) == 0

  describe "computeSaid" do
    it "produces CESR-encoded digest" do
      let said = computeSaid "{\"test\":\"data\"}"
      String.length said `shouldEqual` 44

    it "starts with F (Blake2b code)" do
      let said = computeSaid "{\"test\":\"data\"}"
      String.take 1 said `shouldEqual` "F"

    it "is deterministic" do
      let
        said1 = computeSaid "{\"a\":1}"
        said2 = computeSaid "{\"a\":1}"
      said1 `shouldEqual` said2

    it "differs for different inputs" do
      let
        said1 = computeSaid "{\"a\":1}"
        said2 = computeSaid "{\"a\":2}"
      said1 `shouldNotEqual` said2
