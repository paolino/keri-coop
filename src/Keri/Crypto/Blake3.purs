module Keri.Crypto.Blake3
  ( hash
  , hashSize
  ) where

import Data.ArrayBuffer.Types (Uint8Array)
import FFI.Blake3 as Blake3

hash :: Uint8Array -> Uint8Array
hash = Blake3.hash

hashSize :: Int
hashSize = 32
