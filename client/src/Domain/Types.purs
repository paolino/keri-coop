module Domain.Types
  ( MemberId(..)
  , PurchaseId(..)
  , Cents(..)
  , MemberName(..)
  , Reason(..)
  , PurchaseName(..)
  , AID(..)
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Newtype (class Newtype)

-- | KERI Autonomic Identifier (CESR-encoded prefix)
newtype AID = AID String

derive instance newtypeAID :: Newtype AID _
derive newtype instance eqAID :: Eq AID
derive newtype instance ordAID :: Ord AID
derive newtype instance showAID :: Show AID
derive newtype instance encodeJsonAID :: EncodeJson AID
derive newtype instance decodeJsonAID :: DecodeJson AID

-- | Member identifier (same as AID in our model)
newtype MemberId = MemberId AID

derive instance newtypeMemberId :: Newtype MemberId _
derive newtype instance eqMemberId :: Eq MemberId
derive newtype instance ordMemberId :: Ord MemberId
derive newtype instance showMemberId :: Show MemberId
derive newtype instance encodeJsonMemberId :: EncodeJson MemberId
derive newtype instance decodeJsonMemberId :: DecodeJson MemberId

-- | Purchase identifier (SAID of the OpenPurchase event)
newtype PurchaseId = PurchaseId String

derive instance newtypePurchaseId :: Newtype PurchaseId _
derive newtype instance eqPurchaseId :: Eq PurchaseId
derive newtype instance ordPurchaseId :: Ord PurchaseId
derive newtype instance showPurchaseId :: Show PurchaseId
derive newtype instance encodeJsonPurchaseId :: EncodeJson PurchaseId
derive newtype instance decodeJsonPurchaseId :: DecodeJson PurchaseId

-- | Amount in cents (integer, no floating point)
newtype Cents = Cents Int

derive instance newtypeCents :: Newtype Cents _
derive newtype instance eqCents :: Eq Cents
derive newtype instance ordCents :: Ord Cents
derive newtype instance showCents :: Show Cents
derive newtype instance encodeJsonCents :: EncodeJson Cents
derive newtype instance decodeJsonCents :: DecodeJson Cents
derive newtype instance semiringCents :: Semiring Cents
derive newtype instance ringCents :: Ring Cents

newtype MemberName = MemberName String

derive instance newtypeMemberName :: Newtype MemberName _
derive newtype instance eqMemberName :: Eq MemberName
derive newtype instance showMemberName :: Show MemberName
derive newtype instance encodeJsonMemberName :: EncodeJson MemberName
derive newtype instance decodeJsonMemberName :: DecodeJson MemberName

newtype Reason = Reason String

derive instance newtypeReason :: Newtype Reason _
derive newtype instance eqReason :: Eq Reason
derive newtype instance showReason :: Show Reason
derive newtype instance encodeJsonReason :: EncodeJson Reason
derive newtype instance decodeJsonReason :: DecodeJson Reason

newtype PurchaseName = PurchaseName String

derive instance newtypePurchaseName :: Newtype PurchaseName _
derive newtype instance eqPurchaseName :: Eq PurchaseName
derive newtype instance showPurchaseName :: Show PurchaseName
derive newtype instance encodeJsonPurchaseName :: EncodeJson PurchaseName
derive newtype instance decodeJsonPurchaseName :: DecodeJson PurchaseName
