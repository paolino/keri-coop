module Test.Keri.Cesr.DerivationCodeSpec where

import Prelude

import Data.Either (Either(..))
import Keri.Cesr.DerivationCode
  ( DerivationCode(..)
  , codeLength
  , codeText
  , identifyCode
  , rawSize
  , totalLength
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "DerivationCode" do
  describe "codeText" do
    it "Ed25519PubKey → D" do
      codeText Ed25519PubKey `shouldEqual` "D"

    it "Blake2bDigest → F" do
      codeText Blake2bDigest `shouldEqual` "F"

    it "Ed25519Sig → 0B" do
      codeText Ed25519Sig `shouldEqual` "0B"

  describe "rawSize" do
    it "Ed25519PubKey is 32" do
      rawSize Ed25519PubKey `shouldEqual` 32

    it "Blake2bDigest is 32" do
      rawSize Blake2bDigest `shouldEqual` 32

    it "Ed25519Sig is 64" do
      rawSize Ed25519Sig `shouldEqual` 64

  describe "codeLength" do
    it "Ed25519PubKey is 1" do
      codeLength Ed25519PubKey `shouldEqual` 1

    it "Blake2bDigest is 1" do
      codeLength Blake2bDigest `shouldEqual` 1

    it "Ed25519Sig is 2" do
      codeLength Ed25519Sig `shouldEqual` 2

  describe "totalLength" do
    it "Ed25519PubKey is 44" do
      totalLength Ed25519PubKey `shouldEqual` 44

    it "Blake2bDigest is 44" do
      totalLength Blake2bDigest `shouldEqual` 44

    it "Ed25519Sig is 88" do
      totalLength Ed25519Sig `shouldEqual` 88

  describe "identifyCode" do
    it "identifies D as Ed25519PubKey" do
      identifyCode "Dabc" `shouldEqual` Right Ed25519PubKey

    it "identifies F as Blake2bDigest" do
      identifyCode "Fxyz" `shouldEqual` Right Blake2bDigest

    it "identifies 0B as Ed25519Sig" do
      identifyCode "0Bsig" `shouldEqual` Right Ed25519Sig

    it "rejects unknown codes" do
      case identifyCode "Zxxx" of
        Left _ → pure unit
        Right _ → shouldEqual "expected Left" "got Right"
