module Test.Keri.Cesr.PrimitiveSpec where

import Prelude

import Data.Either (isLeft, isRight)
import FFI.Uint8Array as U8
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Primitive (mkPrimitive)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Cesr.Primitive" do
  describe "mkPrimitive" do
    it "accepts correct size for Ed25519PubKey" do
      isRight (mkPrimitive Ed25519PubKey (U8.zeros 32)) `shouldEqual` true

    it "rejects wrong size for Ed25519PubKey" do
      isLeft (mkPrimitive Ed25519PubKey (U8.zeros 16)) `shouldEqual` true

    it "accepts correct size for Blake2bDigest" do
      isRight (mkPrimitive Blake2bDigest (U8.zeros 32)) `shouldEqual` true

    it "rejects wrong size for Blake2bDigest" do
      isLeft (mkPrimitive Blake2bDigest (U8.zeros 64)) `shouldEqual` true

    it "accepts correct size for Ed25519Sig" do
      isRight (mkPrimitive Ed25519Sig (U8.zeros 64)) `shouldEqual` true

    it "rejects wrong size for Ed25519Sig" do
      isLeft (mkPrimitive Ed25519Sig (U8.zeros 32)) `shouldEqual` true
