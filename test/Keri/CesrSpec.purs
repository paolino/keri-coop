module Test.Keri.CesrSpec where

import Prelude

import Data.Either (Either(..))
import FFI.Uint8Array as U8
import Keri.Cesr.Decode (decode)
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode (encode)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "CESR roundtrip" do
  it "roundtrips Ed25519PubKey (zeros)" do
    let
      raw = U8.zeros 32
      encoded = encode { code: Ed25519PubKey, raw }
    case decode encoded of
      Left err → shouldEqual "expected Right" err
      Right prim → do
        prim.code `shouldEqual` Ed25519PubKey
        U8.length prim.raw `shouldEqual` 32

  it "roundtrips Blake2bDigest (zeros)" do
    let
      raw = U8.zeros 32
      encoded = encode { code: Blake2bDigest, raw }
    case decode encoded of
      Left err → shouldEqual "expected Right" err
      Right prim → prim.code `shouldEqual` Blake2bDigest

  it "roundtrips Ed25519Sig (zeros)" do
    let
      raw = U8.zeros 64
      encoded = encode { code: Ed25519Sig, raw }
    case decode encoded of
      Left err → shouldEqual "expected Right" err
      Right prim → do
        prim.code `shouldEqual` Ed25519Sig
        U8.length prim.raw `shouldEqual` 64

  it "encode then decode preserves raw bytes" do
    let
      raw = U8.zeros 32
      encoded = encode { code: Ed25519PubKey, raw }
    case decode encoded of
      Left err → shouldEqual "expected Right" err
      Right prim → do
        let reEncoded = encode prim
        reEncoded `shouldEqual` encoded
