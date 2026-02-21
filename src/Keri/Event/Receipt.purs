module Keri.Event.Receipt
  ( ReceiptConfig
  , mkReceipt
  ) where

import Data.String as String
import Keri.Crypto.Digest (computeSaid, saidPlaceholder)
import Keri.Event (Event(..), ReceiptData)
import Keri.Event.Serialize (serializeEvent)
import Keri.Event.Version (mkVersion, versionPlaceholder)

type ReceiptConfig =
  { prefix :: String
  , sequenceNumber :: Int
  }

mkReceipt :: ReceiptConfig -> Event
mkReceipt cfg = Receipt finalData
  where
  placeholder :: ReceiptData
  placeholder =
    { version: versionPlaceholder
    , digest: saidPlaceholder
    , prefix: cfg.prefix
    , sequenceNumber: cfg.sequenceNumber
    }

  size0 = String.length (serializeEvent (Receipt placeholder))
  realVersion = mkVersion size0
  withVersion = placeholder { version = realVersion }
  saidJson = serializeEvent (Receipt withVersion)
  said = computeSaid saidJson
  finalData = withVersion { digest = said }
