module Keri.Crypto.SAID
  ( verifySaid
  , replaceDigest
  ) where

import Prelude

import Keri.Crypto.Digest (computeSaid, saidPlaceholder)
import Keri.Event (Event(..), eventDigest)
import Keri.Event.Serialize (serializeEvent)

-- | Replace the digest field with the SAID placeholder.
-- For inception events, both digest and prefix are replaced
-- (because the prefix is derived from the SAID).
replaceDigest :: Event -> Event
replaceDigest (Inception d) = Inception (d { digest = saidPlaceholder, prefix = saidPlaceholder })
replaceDigest (Rotation d) = Rotation (d { digest = saidPlaceholder })
replaceDigest (Interaction d) = Interaction (d { digest = saidPlaceholder })
replaceDigest (Receipt d) = Receipt (d { digest = saidPlaceholder })

-- | Verify that the event's claimed SAID matches the recomputed one.
verifySaid :: Event -> Boolean
verifySaid evt = computeSaid (serializeEvent (replaceDigest evt)) == eventDigest evt
