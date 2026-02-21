module Test.Keri.Cesr.EncodeSpec where

import Prelude

import Data.String as String
import FFI.Uint8Array as U8
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode (encode)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "Cesr.Encode" do
  describe "encode" do
    it "encodes a 32-byte key to 44 chars" do
      let
        raw = U8.zeros 32
        result = encode { code: Ed25519PubKey, raw }
      String.length result `shouldEqual` 44

    it "starts with D for Ed25519PubKey" do
      let result = encode { code: Ed25519PubKey, raw: U8.zeros 32 }
      String.take 1 result `shouldEqual` "D"

    it "encodes a 32-byte digest to 44 chars" do
      let result = encode { code: Blake2bDigest, raw: U8.zeros 32 }
      String.length result `shouldEqual` 44

    it "starts with F for Blake2bDigest" do
      let result = encode { code: Blake2bDigest, raw: U8.zeros 32 }
      String.take 1 result `shouldEqual` "F"

    it "encodes a 64-byte signature to 88 chars" do
      let result = encode { code: Ed25519Sig, raw: U8.zeros 64 }
      String.length result `shouldEqual` 88

    it "starts with 0B for Ed25519Sig" do
      let result = encode { code: Ed25519Sig, raw: U8.zeros 64 }
      String.take 2 result `shouldEqual` "0B"

    it "produces Base64url-safe characters" do
      let result = encode { code: Ed25519PubKey, raw: U8.zeros 32 }
      result `shouldSatisfy` \s →
        not (String.contains (String.Pattern "+") s)
          && not (String.contains (String.Pattern "/") s)
          && not (String.contains (String.Pattern "=") s)
