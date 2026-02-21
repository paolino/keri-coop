module Keri.Crypto.Ed25519
  ( generateKeyPair
  , sign
  , verify
  , publicKeyBytes
  , secretKeyBytes
  ) where

import Data.ArrayBuffer.Types (Uint8Array)
import Effect (Effect)
import FFI.TweetNaCl as NaCl

generateKeyPair :: Effect { publicKey :: Uint8Array, secretKey :: Uint8Array }
generateKeyPair = NaCl.generateKeyPair

sign :: Uint8Array -> Uint8Array -> Uint8Array
sign = NaCl.sign

verify :: Uint8Array -> Uint8Array -> Uint8Array -> Boolean
verify = NaCl.verify

publicKeyBytes :: { publicKey :: Uint8Array, secretKey :: Uint8Array } -> Uint8Array
publicKeyBytes kp = kp.publicKey

secretKeyBytes :: { publicKey :: Uint8Array, secretKey :: Uint8Array } -> Uint8Array
secretKeyBytes kp = kp.secretKey
