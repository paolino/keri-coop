module Domain.State
  ( GroupState
  , PurchaseState
  , CommitmentStatus(..)
  , PurchasePhase(..)
  , SignedEvent
  , emptyState
  , applySignedEvent
  , replaySignedEvents
  , adminCount
  , quorum
  , isAdmin
  ) where

import Prelude

import Data.Array (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Domain.Event (DomainEvent(..))
import Domain.Types (Cents(..), MemberId, MemberName, PurchaseId(..), PurchaseName)

-- | A domain event together with its signer and event identifier.
-- | The eventId is the SAID of the KERI interaction event.
type SignedEvent =
  { signer :: MemberId
  , eventId :: String
  , event :: DomainEvent
  }

-- | Commitment approval status within a purchase.
data CommitmentStatus = Pending | Approved | Rejected

derive instance eqCommitmentStatus :: Eq CommitmentStatus

-- | Purchase lifecycle phase.
data PurchasePhase = Open | Closed | Failed

derive instance eqPurchasePhase :: Eq PurchasePhase

-- | State of a single purchase.
type PurchaseState =
  { name :: PurchaseName
  , referente :: MemberId
  , phase :: PurchasePhase
  , commitments :: Map MemberId { amount :: Cents, status :: CommitmentStatus }
  , closeVotes :: Set MemberId
  , failVotes :: Set MemberId
  }

-- | Full group state, derived by folding signed domain events.
type GroupState =
  { members :: Map MemberId MemberName
  , referenti :: Set MemberId
  , cassieri :: Set MemberId
  , balances :: Map MemberId Cents
  , purchases :: Map PurchaseId PurchaseState
  , voteTallies :: Map String (Set MemberId)
  }

emptyState :: GroupState
emptyState =
  { members: Map.empty
  , referenti: Set.empty
  , cassieri: Set.empty
  , balances: Map.empty
  , purchases: Map.empty
  , voteTallies: Map.empty
  }

-- | Number of admins (referenti + cassieri, unified).
adminCount :: GroupState -> Int
adminCount st = Set.size (Set.union st.referenti st.cassieri)

-- | Majority quorum threshold.
quorum :: GroupState -> Int
quorum st = (adminCount st + 1) / 2

-- | Check if a member is an admin (referente or cassiere).
isAdmin :: MemberId -> GroupState -> Boolean
isAdmin mid st =
  Set.member mid st.referenti || Set.member mid st.cassieri

-- | Apply a signed domain event to the group state.
-- | The signer's MemberId is used for vote tracking and purchase ownership.
applySignedEvent :: SignedEvent -> GroupState -> GroupState
applySignedEvent { signer, eventId, event } st = case event of
  -- Governance votes
  VoteRegisterMember name ->
    applyVote signer ("RegisterMember:" <> show name) st \s ->
      s
        { members = Map.insert signer name s.members
        , balances = Map.insert signer (Cents 0) s.balances
        }

  VoteRemoveMember mid ->
    applyVote signer ("RemoveMember:" <> show mid) st \s ->
      s
        { members = Map.delete mid s.members
        , referenti = Set.delete mid s.referenti
        , cassieri = Set.delete mid s.cassieri
        , balances = Map.delete mid s.balances
        }

  VoteElectReferente mid ->
    applyVote signer ("ElectReferente:" <> show mid) st \s ->
      s { referenti = Set.insert mid s.referenti }

  VoteRevokeReferente mid ->
    applyVote signer ("RevokeReferente:" <> show mid) st \s ->
      s { referenti = Set.delete mid s.referenti }

  VoteElectCassiere mid ->
    applyVote signer ("ElectCassiere:" <> show mid) st \s ->
      s { cassieri = Set.insert mid s.cassieri }

  VoteRevokeCassiere mid ->
    applyVote signer ("RevokeCassiere:" <> show mid) st \s ->
      s { cassieri = Set.delete mid s.cassieri }

  -- Economics
  Deposit mid cents ->
    st { balances = Map.alter (addBalance cents) mid st.balances }

  Withdraw mid cents _ ->
    st { balances = Map.alter (subBalance cents) mid st.balances }

  -- Purchases
  OpenPurchase name ->
    let
      pid = PurchaseId eventId
      ps =
        { name
        , referente: signer
        , phase: Open
        , commitments: Map.empty
        , closeVotes: Set.empty
        , failVotes: Set.empty
        }
    in
      st { purchases = Map.insert pid ps st.purchases }

  Commit mid cents pid ->
    let
      st' = st { balances = Map.alter (subBalance cents) mid st.balances }
    in
      updatePurchase pid st' \ps ->
        ps { commitments = Map.insert mid { amount: cents, status: Pending } ps.commitments }

  ApproveCommitment mid pid ->
    updatePurchase pid st \ps ->
      ps { commitments = Map.update (\c -> Just c { status = Approved }) mid ps.commitments }

  RejectCommitment mid pid ->
    let
      refundAmount = case Map.lookup pid st.purchases of
        Just ps -> case Map.lookup mid ps.commitments of
          Just c -> c.amount
          Nothing -> Cents 0
        Nothing -> Cents 0
      st' = st { balances = Map.alter (addBalance refundAmount) mid st.balances }
    in
      updatePurchase pid st' \ps ->
        ps { commitments = Map.update (\c -> Just c { status = Rejected }) mid ps.commitments }

  AdjustCommitment mid cents pid ->
    let
      oldAmount = case Map.lookup pid st.purchases of
        Just ps -> case Map.lookup mid ps.commitments of
          Just c -> c.amount
          Nothing -> Cents 0
        Nothing -> Cents 0
      diff = oldAmount - cents
      st' = st { balances = Map.alter (addBalance diff) mid st.balances }
    in
      updatePurchase pid st' \ps ->
        ps { commitments = Map.update (\c -> Just c { amount = cents }) mid ps.commitments }

  VoteClosePurchase pid ->
    updatePurchase pid st \ps ->
      ps { closeVotes = Set.insert signer ps.closeVotes }

  VoteFailPurchase pid ->
    updatePurchase pid st \ps ->
      ps { failVotes = Set.insert signer ps.failVotes }

  ClosePurchase pid ->
    updatePurchase pid st \ps ->
      ps { phase = Closed, closeVotes = Set.empty, failVotes = Set.empty }

  FailPurchase pid ->
    refundAllCommitments pid $
      updatePurchase pid st \ps ->
        ps { phase = Failed, closeVotes = Set.empty, failVotes = Set.empty }

-- | Replay a sequence of signed events from empty state.
replaySignedEvents :: Array SignedEvent -> GroupState
replaySignedEvents = foldl (flip applySignedEvent) emptyState

-- Internal helpers

addBalance :: Cents -> Maybe Cents -> Maybe Cents
addBalance c existing = Just (fromMaybe (Cents 0) existing + c)

subBalance :: Cents -> Maybe Cents -> Maybe Cents
subBalance c existing = Just (fromMaybe (Cents 0) existing - c)

updatePurchase
  :: PurchaseId
  -> GroupState
  -> (PurchaseState -> PurchaseState)
  -> GroupState
updatePurchase pid st f =
  st { purchases = Map.update (Just <<< f) pid st.purchases }

-- | Refund all approved commitments for a failed purchase.
refundAllCommitments :: PurchaseId -> GroupState -> GroupState
refundAllCommitments pid st = case Map.lookup pid st.purchases of
  Nothing -> st
  Just ps ->
    let
      entries :: Array (Tuple MemberId { amount :: Cents, status :: CommitmentStatus })
      entries = Map.toUnfoldable ps.commitments
      refund bals (Tuple mid c)
        | c.status == Approved || c.status == Pending =
            Map.alter (addBalance c.amount) mid bals
        | otherwise = bals
      newBalances = foldl refund st.balances entries
    in
      st { balances = newBalances }

-- | Apply a governance vote. If quorum is reached, execute the action
-- | and clear the tally.
applyVote
  :: MemberId
  -> String
  -> GroupState
  -> (GroupState -> GroupState)
  -> GroupState
applyVote voter key st action =
  let
    tally = fromMaybe Set.empty (Map.lookup key st.voteTallies)
    newTally = Set.insert voter tally
    q = quorum st
  in
    if Set.size newTally >= q then
      (action st) { voteTallies = Map.delete key st.voteTallies }
    else
      st { voteTallies = Map.insert key newTally st.voteTallies }
