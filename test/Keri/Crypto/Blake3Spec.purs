module Test.Keri.Crypto.Blake3Spec where

import Prelude

import FFI.Base64Url as B64
import FFI.TextEncoder (encodeUtf8)
import FFI.Uint8Array as U8
import Keri.Crypto.Blake3 (hash, hashSize)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

spec :: Spec Unit
spec = describe "Crypto.Blake3" do
  describe "hash" do
    it "produces 32 bytes" do
      let h = hash (encodeUtf8 "hello")
      U8.length h `shouldEqual` 32

    it "is deterministic" do
      let
        h1 = hash (encodeUtf8 "test input")
        h2 = hash (encodeUtf8 "test input")
      B64.encode h1 `shouldEqual` B64.encode h2

    it "differs for different inputs" do
      let
        h1 = hash (encodeUtf8 "input a")
        h2 = hash (encodeUtf8 "input b")
      B64.encode h1 `shouldNotEqual` B64.encode h2

  describe "hashSize" do
    it "is 32" do
      hashSize `shouldEqual` 32
