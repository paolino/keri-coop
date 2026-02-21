module Test.Keri.Crypto.Ed25519Spec where

import Prelude

import Effect.Class (liftEffect)
import FFI.Base64Url as B64
import FFI.TextEncoder (encodeUtf8)
import FFI.Uint8Array as U8
import Keri.Crypto.Ed25519
  ( generateKeyPair
  , publicKeyBytes
  , secretKeyBytes
  , sign
  , verify
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

spec :: Spec Unit
spec = describe "Crypto.Ed25519" do
  describe "generateKeyPair" do
    it "produces 32-byte public key" do
      kp ← liftEffect generateKeyPair
      U8.length (publicKeyBytes kp) `shouldEqual` 32

    it "produces 64-byte secret key" do
      kp ← liftEffect generateKeyPair
      U8.length (secretKeyBytes kp) `shouldEqual` 64

    it "generates distinct key pairs" do
      kp1 ← liftEffect generateKeyPair
      kp2 ← liftEffect generateKeyPair
      B64.encode (publicKeyBytes kp1) `shouldNotEqual` B64.encode (publicKeyBytes kp2)

  describe "sign and verify" do
    it "verifies a valid signature" do
      kp ← liftEffect generateKeyPair
      let
        msg = encodeUtf8 "hello keri"
        sig = sign msg (secretKeyBytes kp)
      verify msg sig (publicKeyBytes kp) `shouldEqual` true

    it "rejects wrong message" do
      kp ← liftEffect generateKeyPair
      let sig = sign (encodeUtf8 "hello") (secretKeyBytes kp)
      verify (encodeUtf8 "wrong") sig (publicKeyBytes kp) `shouldEqual` false

    it "rejects wrong key" do
      kp1 ← liftEffect generateKeyPair
      kp2 ← liftEffect generateKeyPair
      let sig = sign (encodeUtf8 "hello") (secretKeyBytes kp1)
      verify (encodeUtf8 "hello") sig (publicKeyBytes kp2) `shouldEqual` false
