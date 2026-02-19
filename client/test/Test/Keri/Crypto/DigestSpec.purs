module Test.Keri.Crypto.DigestSpec where

import Prelude

import Data.String as String
import Keri.Cesr.DerivationCode (DerivationCode(..), totalLength)
import Keri.Crypto.Digest (computeSaid, saidPlaceholder)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "Crypto.Digest" do
  it "saidPlaceholder has correct length" do
    String.length saidPlaceholder `shouldEqual` totalLength Blake3Digest

  it "saidPlaceholder is all #" do
    saidPlaceholder `shouldSatisfy` \s ->
      String.replaceAll (String.Pattern "#") (String.Replacement "") s == ""

  it "computeSaid returns CESR-encoded Blake3 digest" do
    let said = computeSaid "{\"hello\":\"world\"}"
    said `shouldSatisfy` \s -> String.take 1 s == "E"
    String.length said `shouldEqual` totalLength Blake3Digest

  it "computeSaid is deterministic" do
    let
      input = "{\"test\":\"data\"}"
      said1 = computeSaid input
      said2 = computeSaid input
    said1 `shouldEqual` said2

  it "different inputs produce different SAIDs" do
    let
      said1 = computeSaid "{\"a\":1}"
      said2 = computeSaid "{\"a\":2}"
    said1 `shouldSatisfy` \s -> s /= said2
