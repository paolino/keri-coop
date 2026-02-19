module Domain.Event
  ( DomainEvent(..)
  , serializeDomainEvent
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Domain.Types (Cents(..), MemberId, MemberName(..), PurchaseId(..), PurchaseName(..), Reason(..))
import Json.Canonical (jsonStr, jsonInt, jsonObj, kv)

-- | Domain events for the cooperative purchasing group.
data DomainEvent
  -- Governance (require admin vote)
  = VoteRegisterMember MemberName
  | VoteRemoveMember MemberId
  | VoteElectReferente MemberId
  | VoteRevokeReferente MemberId
  | VoteElectCassiere MemberId
  | VoteRevokeCassiere MemberId
  -- Economics (signed by cassiere)
  | Deposit MemberId Cents
  | Withdraw MemberId Cents Reason
  -- Purchases
  | OpenPurchase PurchaseName
  | Commit MemberId Cents PurchaseId
  | ApproveCommitment MemberId PurchaseId
  | RejectCommitment MemberId PurchaseId
  | AdjustCommitment MemberId Cents PurchaseId
  | VoteClosePurchase PurchaseId
  | VoteFailPurchase PurchaseId
  | ClosePurchase PurchaseId
  | FailPurchase PurchaseId

derive instance eqDomainEvent :: Eq DomainEvent

-- | Canonical JSON serialization with deterministic field order.
-- | Used for signing — the bytes signed must be reproducible.
serializeDomainEvent :: DomainEvent -> String
serializeDomainEvent = case _ of
  VoteRegisterMember (MemberName name) -> jsonObj
    [ kv "name" (jsonStr name)
    , kv "t" (jsonStr "VoteRegisterMember")
    ]
  VoteRemoveMember mid -> jsonObj
    [ kv "memberId" (midStr mid)
    , kv "t" (jsonStr "VoteRemoveMember")
    ]
  VoteElectReferente mid -> jsonObj
    [ kv "memberId" (midStr mid)
    , kv "t" (jsonStr "VoteElectReferente")
    ]
  VoteRevokeReferente mid -> jsonObj
    [ kv "memberId" (midStr mid)
    , kv "t" (jsonStr "VoteRevokeReferente")
    ]
  VoteElectCassiere mid -> jsonObj
    [ kv "memberId" (midStr mid)
    , kv "t" (jsonStr "VoteElectCassiere")
    ]
  VoteRevokeCassiere mid -> jsonObj
    [ kv "memberId" (midStr mid)
    , kv "t" (jsonStr "VoteRevokeCassiere")
    ]
  Deposit mid (Cents cents) -> jsonObj
    [ kv "cents" (jsonInt cents)
    , kv "memberId" (midStr mid)
    , kv "t" (jsonStr "Deposit")
    ]
  Withdraw mid (Cents cents) (Reason reason) -> jsonObj
    [ kv "cents" (jsonInt cents)
    , kv "memberId" (midStr mid)
    , kv "reason" (jsonStr reason)
    , kv "t" (jsonStr "Withdraw")
    ]
  OpenPurchase (PurchaseName name) -> jsonObj
    [ kv "name" (jsonStr name)
    , kv "t" (jsonStr "OpenPurchase")
    ]
  Commit mid (Cents cents) (PurchaseId pid) -> jsonObj
    [ kv "cents" (jsonInt cents)
    , kv "memberId" (midStr mid)
    , kv "purchaseId" (jsonStr pid)
    , kv "t" (jsonStr "Commit")
    ]
  ApproveCommitment mid (PurchaseId pid) -> jsonObj
    [ kv "memberId" (midStr mid)
    , kv "purchaseId" (jsonStr pid)
    , kv "t" (jsonStr "ApproveCommitment")
    ]
  RejectCommitment mid (PurchaseId pid) -> jsonObj
    [ kv "memberId" (midStr mid)
    , kv "purchaseId" (jsonStr pid)
    , kv "t" (jsonStr "RejectCommitment")
    ]
  AdjustCommitment mid (Cents cents) (PurchaseId pid) -> jsonObj
    [ kv "cents" (jsonInt cents)
    , kv "memberId" (midStr mid)
    , kv "purchaseId" (jsonStr pid)
    , kv "t" (jsonStr "AdjustCommitment")
    ]
  VoteClosePurchase (PurchaseId pid) -> jsonObj
    [ kv "purchaseId" (jsonStr pid)
    , kv "t" (jsonStr "VoteClosePurchase")
    ]
  VoteFailPurchase (PurchaseId pid) -> jsonObj
    [ kv "purchaseId" (jsonStr pid)
    , kv "t" (jsonStr "VoteFailPurchase")
    ]
  ClosePurchase (PurchaseId pid) -> jsonObj
    [ kv "purchaseId" (jsonStr pid)
    , kv "t" (jsonStr "ClosePurchase")
    ]
  FailPurchase (PurchaseId pid) -> jsonObj
    [ kv "purchaseId" (jsonStr pid)
    , kv "t" (jsonStr "FailPurchase")
    ]

midStr :: MemberId -> String
midStr mid = jsonStr (unwrap (unwrap mid))

instance decodeJsonDomainEvent :: DecodeJson DomainEvent where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .: "t"
    case tag of
      "VoteRegisterMember" -> VoteRegisterMember <$> obj .: "name"
      "VoteRemoveMember" -> VoteRemoveMember <$> obj .: "memberId"
      "VoteElectReferente" -> VoteElectReferente <$> obj .: "memberId"
      "VoteRevokeReferente" -> VoteRevokeReferente <$> obj .: "memberId"
      "VoteElectCassiere" -> VoteElectCassiere <$> obj .: "memberId"
      "VoteRevokeCassiere" -> VoteRevokeCassiere <$> obj .: "memberId"
      "Deposit" -> Deposit <$> obj .: "memberId" <*> obj .: "cents"
      "Withdraw" -> Withdraw <$> obj .: "memberId" <*> obj .: "cents" <*> obj .: "reason"
      "OpenPurchase" -> OpenPurchase <$> obj .: "name"
      "Commit" -> Commit <$> obj .: "memberId" <*> obj .: "cents" <*> obj .: "purchaseId"
      "ApproveCommitment" -> ApproveCommitment <$> obj .: "memberId" <*> obj .: "purchaseId"
      "RejectCommitment" -> RejectCommitment <$> obj .: "memberId" <*> obj .: "purchaseId"
      "AdjustCommitment" -> AdjustCommitment <$> obj .: "memberId" <*> obj .: "cents" <*> obj .: "purchaseId"
      "VoteClosePurchase" -> VoteClosePurchase <$> obj .: "purchaseId"
      "VoteFailPurchase" -> VoteFailPurchase <$> obj .: "purchaseId"
      "ClosePurchase" -> ClosePurchase <$> obj .: "purchaseId"
      "FailPurchase" -> FailPurchase <$> obj .: "purchaseId"
      _ -> Left (TypeMismatch ("Unknown domain event type: " <> tag))
