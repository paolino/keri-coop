module Test.Keri.Cesr.DecodeSpec where

import Prelude

import Data.Either (Either(..))
import FFI.Uint8Array as U8
import Keri.Cesr.Decode (decode)
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode (encode)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Cesr.Decode" do
  describe "decode" do
    it "roundtrips Ed25519PubKey" do
      let
        raw = U8.zeros 32
        encoded = encode { code: Ed25519PubKey, raw }
      case decode encoded of
        Left err → shouldEqual "expected Right" err
        Right prim → do
          prim.code `shouldEqual` Ed25519PubKey
          U8.length prim.raw `shouldEqual` 32

    it "roundtrips Blake2bDigest" do
      let
        raw = U8.zeros 32
        encoded = encode { code: Blake2bDigest, raw }
      case decode encoded of
        Left err → shouldEqual "expected Right" err
        Right prim → do
          prim.code `shouldEqual` Blake2bDigest
          U8.length prim.raw `shouldEqual` 32

    it "roundtrips Ed25519Sig" do
      let
        raw = U8.zeros 64
        encoded = encode { code: Ed25519Sig, raw }
      case decode encoded of
        Left err → shouldEqual "expected Right" err
        Right prim → do
          prim.code `shouldEqual` Ed25519Sig
          U8.length prim.raw `shouldEqual` 64

    it "rejects too-short input" do
      case decode "D" of
        Left _ → pure unit
        Right _ → shouldEqual "Left" "Right"

    it "rejects unknown codes" do
      case decode "Zxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" of
        Left _ → pure unit
        Right _ → shouldEqual "Left" "Right"
