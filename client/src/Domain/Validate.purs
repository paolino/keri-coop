module Domain.Validate
  ( ValidationError(..)
  , validateEvent
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Domain.Event (DomainEvent(..))
import Domain.State (GroupState, PurchasePhase(..), isAdmin)
import Domain.Types (Cents(..), MemberId, PurchaseId)

-- | Validation errors for domain event authorization.
data ValidationError
  = NotAnAdmin MemberId
  | NotACassiere MemberId
  | NotAReferente MemberId
  | MemberNotFound MemberId
  | PurchaseNotFound PurchaseId
  | PurchaseNotOpen PurchaseId
  | InsufficientBalance MemberId

derive instance eqValidationError :: Eq ValidationError

-- | Validate that a signer is authorized to submit an event
-- | given the current group state. Returns Nothing if valid.
validateEvent
  :: MemberId
  -> DomainEvent
  -> GroupState
  -> Maybe ValidationError
validateEvent signer event st = case event of
  VoteRegisterMember _ -> requireAdmin signer st
  VoteRemoveMember _ -> requireAdmin signer st
  VoteElectReferente _ -> requireAdmin signer st
  VoteRevokeReferente _ -> requireAdmin signer st
  VoteElectCassiere _ -> requireAdmin signer st
  VoteRevokeCassiere _ -> requireAdmin signer st

  Deposit _ _ -> requireCassiere signer st
  Withdraw mid _ _ ->
    requireCassiere signer st
      <|> requireMemberExists mid st

  OpenPurchase _ -> requireReferente signer st

  Commit mid cents pid ->
    requireMemberExists mid st
      <|> requirePurchaseOpen pid st
      <|> requireBalance mid cents st

  ApproveCommitment _ pid ->
    requireReferente signer st
      <|> requirePurchaseOpen pid st

  RejectCommitment _ pid ->
    requireReferente signer st
      <|> requirePurchaseOpen pid st

  AdjustCommitment _ _ pid ->
    requireReferente signer st
      <|> requirePurchaseOpen pid st

  VoteClosePurchase pid ->
    requireAdmin signer st
      <|> requirePurchaseOpen pid st

  VoteFailPurchase pid ->
    requireAdmin signer st
      <|> requirePurchaseOpen pid st

  ClosePurchase pid ->
    requireReferente signer st
      <|> requirePurchaseOpen pid st

  FailPurchase pid ->
    requireReferente signer st
      <|> requirePurchaseOpen pid st

-- Internal helpers

requireAdmin :: MemberId -> GroupState -> Maybe ValidationError
requireAdmin mid st
  | isAdmin mid st = Nothing
  | otherwise = Just (NotAnAdmin mid)

requireReferente :: MemberId -> GroupState -> Maybe ValidationError
requireReferente mid st
  | Set.member mid st.referenti = Nothing
  | otherwise = Just (NotAReferente mid)

requireCassiere :: MemberId -> GroupState -> Maybe ValidationError
requireCassiere mid st
  | Set.member mid st.cassieri = Nothing
  | otherwise = Just (NotACassiere mid)

requireMemberExists :: MemberId -> GroupState -> Maybe ValidationError
requireMemberExists mid st
  | Map.member mid st.members = Nothing
  | otherwise = Just (MemberNotFound mid)

requirePurchaseOpen :: PurchaseId -> GroupState -> Maybe ValidationError
requirePurchaseOpen pid st = case Map.lookup pid st.purchases of
  Nothing -> Just (PurchaseNotFound pid)
  Just ps
    | ps.phase /= Open -> Just (PurchaseNotOpen pid)
    | otherwise -> Nothing

requireBalance :: MemberId -> Cents -> GroupState -> Maybe ValidationError
requireBalance mid (Cents needed) st = case Map.lookup mid st.balances of
  Nothing -> Just (MemberNotFound mid)
  Just (Cents bal)
    | bal < needed -> Just (InsufficientBalance mid)
    | otherwise -> Nothing
