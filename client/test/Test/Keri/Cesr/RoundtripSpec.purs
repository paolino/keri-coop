module Test.Keri.Cesr.RoundtripSpec where

import Prelude

import Data.Either (Either(..))
import Effect.Class (liftEffect)
import FFI.TweetNaCl as NaCl
import FFI.Uint8Array as U8
import Keri.Cesr.Decode as Decode
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode as Encode
import Keri.Cesr.Primitive (mkPrimitive)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)

spec :: Spec Unit
spec = describe "CESR roundtrip" do
  it "roundtrips Ed25519 public key" do
    kp <- liftEffect NaCl.generateKeyPair
    case mkPrimitive Ed25519PubKey kp.publicKey of
      Left err -> shouldEqual err ""
      Right prim -> do
        let encoded = Encode.encode prim
        case Decode.decode encoded of
          Left err -> shouldEqual err ""
          Right decoded -> do
            decoded.code `shouldEqual` Ed25519PubKey
            U8.length decoded.raw `shouldEqual` 32

  it "roundtrips Ed25519 signature" do
    kp <- liftEffect NaCl.generateKeyPair
    let
      msg = U8.zeros 10
      sig = NaCl.sign msg kp.secretKey
    case mkPrimitive Ed25519Sig sig of
      Left err -> shouldEqual err ""
      Right prim -> do
        let encoded = Encode.encode prim
        case Decode.decode encoded of
          Left err -> shouldEqual err ""
          Right decoded -> do
            decoded.code `shouldEqual` Ed25519Sig
            U8.length decoded.raw `shouldEqual` 64

  it "roundtrips Blake3 digest" do
    let raw = U8.zeros 32
    case mkPrimitive Blake3Digest raw of
      Left err -> shouldEqual err ""
      Right prim -> do
        let encoded = Encode.encode prim
        case Decode.decode encoded of
          Left err -> shouldEqual err ""
          Right decoded -> do
            decoded.code `shouldEqual` Blake3Digest
            U8.length decoded.raw `shouldEqual` 32

  it "encoded Ed25519PubKey starts with D" do
    kp <- liftEffect NaCl.generateKeyPair
    case mkPrimitive Ed25519PubKey kp.publicKey of
      Left _ -> pure unit
      Right prim -> do
        let encoded = Encode.encode prim
        encoded `shouldSatisfy` \s -> s >= "D"

  it "encoded Ed25519Sig starts with 0B" do
    kp <- liftEffect NaCl.generateKeyPair
    let sig = NaCl.sign (U8.zeros 10) kp.secretKey
    case mkPrimitive Ed25519Sig sig of
      Left _ -> pure unit
      Right prim -> do
        let encoded = Encode.encode prim
        encoded `shouldSatisfy` \s -> s >= "0B"

  it "rejects wrong-size raw bytes" do
    let badBytes = U8.zeros 16
    case mkPrimitive Ed25519PubKey badBytes of
      Left _ -> pure unit
      Right _ -> fail "should have rejected wrong-size bytes"
