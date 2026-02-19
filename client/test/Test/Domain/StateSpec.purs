module Test.Domain.StateSpec where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Domain.Event (DomainEvent(..))
import Domain.State (GroupState, PurchasePhase(..), applySignedEvent, emptyState, quorum, replaySignedEvents, SignedEvent)
import Domain.Types (AID(..), Cents(..), MemberId(..), MemberName(..), PurchaseId(..), PurchaseName(..), Reason(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

mkSigned :: MemberId -> String -> DomainEvent -> SignedEvent
mkSigned signer eventId event = { signer, eventId, event }

alice :: MemberId
alice = MemberId (AID "alice-aid")

bob :: MemberId
bob = MemberId (AID "bob-aid")

carol :: MemberId
carol = MemberId (AID "carol-aid")

-- Bootstrap a state with alice as referente + cassiere
bootstrapped :: GroupState
bootstrapped = replaySignedEvents
  [ mkSigned alice "ev1" (VoteRegisterMember (MemberName "Alice"))
  , mkSigned alice "ev2" (VoteElectReferente alice)
  , mkSigned alice "ev3" (VoteElectCassiere alice)
  ]

spec :: Spec Unit
spec = describe "Domain.State" do
  describe "governance voting" do
    it "registers a member on single-admin quorum" do
      let
        st = applySignedEvent
          (mkSigned alice "ev1" (VoteRegisterMember (MemberName "Alice")))
          emptyState
      Map.lookup alice st.members `shouldEqual` Just (MemberName "Alice")
      Map.lookup alice st.balances `shouldEqual` Just (Cents 0)

    it "elects referente" do
      let st = bootstrapped
      Set.member alice st.referenti `shouldEqual` true

    it "elects cassiere" do
      let st = bootstrapped
      Set.member alice st.cassieri `shouldEqual` true

    it "quorum is 1 for single admin" do
      quorum bootstrapped `shouldEqual` 1

    it "requires 2 votes with 2 admins" do
      let
        st = replaySignedEvents
          [ mkSigned alice "ev1" (VoteRegisterMember (MemberName "Alice"))
          , mkSigned alice "ev2" (VoteElectReferente alice)
          , mkSigned alice "ev3" (VoteElectCassiere alice)
          , mkSigned alice "ev4" (VoteRegisterMember (MemberName "Bob"))
          , mkSigned alice "ev5" (VoteElectReferente bob)
          ]
      quorum st `shouldEqual` 1
    -- Now with both as admins (referente + cassiere = 2 unique)
    -- alice is referente+cassiere, bob is referente → admin set is {alice, bob}
    -- quorum = (2+1)/2 = 1

    it "vote tally clears on quorum" do
      let
        st = replaySignedEvents
          [ mkSigned alice "ev1" (VoteRegisterMember (MemberName "Alice"))
          , mkSigned alice "ev2" (VoteElectReferente alice)
          ]
      Map.lookup "RegisterMember:(MemberName \"Alice\")" st.voteTallies `shouldEqual` Nothing

  describe "economics" do
    it "deposits increase balance" do
      let
        st = applySignedEvent
          (mkSigned alice "ev4" (Deposit alice (Cents 1000)))
          bootstrapped
      Map.lookup alice st.balances `shouldEqual` Just (Cents 1000)

    it "withdrawals decrease balance" do
      let
        st = replaySignedEvents
          [ mkSigned alice "ev1" (VoteRegisterMember (MemberName "Alice"))
          , mkSigned alice "ev2" (VoteElectReferente alice)
          , mkSigned alice "ev3" (VoteElectCassiere alice)
          , mkSigned alice "ev4" (Deposit alice (Cents 1000))
          , mkSigned alice "ev5" (Withdraw alice (Cents 300) (Reason "test"))
          ]
      Map.lookup alice st.balances `shouldEqual` Just (Cents 700)

  describe "purchases" do
    it "opens a purchase" do
      let
        st = applySignedEvent
          (mkSigned alice "pur1" (OpenPurchase (PurchaseName "Oranges")))
          bootstrapped
      Map.size st.purchases `shouldEqual` 1
      case Map.lookup (PurchaseId "pur1") st.purchases of
        Nothing -> shouldEqual "found" "not found"
        Just ps -> do
          ps.phase `shouldEqual` Open
          ps.referente `shouldEqual` alice

    it "commits to a purchase and debits balance" do
      let
        st = replaySignedEvents
          [ mkSigned alice "ev1" (VoteRegisterMember (MemberName "Alice"))
          , mkSigned alice "ev2" (VoteElectReferente alice)
          , mkSigned alice "ev3" (VoteElectCassiere alice)
          , mkSigned alice "ev4" (Deposit alice (Cents 1000))
          , mkSigned alice "pur1" (OpenPurchase (PurchaseName "Oranges"))
          , mkSigned alice "ev5" (Commit alice (Cents 200) (PurchaseId "pur1"))
          ]
      Map.lookup alice st.balances `shouldEqual` Just (Cents 800)
      case Map.lookup (PurchaseId "pur1") st.purchases of
        Nothing -> shouldEqual "found" "not found"
        Just ps -> Map.size ps.commitments `shouldEqual` 1

    it "closes a purchase" do
      let
        st = replaySignedEvents
          [ mkSigned alice "ev1" (VoteRegisterMember (MemberName "Alice"))
          , mkSigned alice "ev2" (VoteElectReferente alice)
          , mkSigned alice "ev3" (VoteElectCassiere alice)
          , mkSigned alice "pur1" (OpenPurchase (PurchaseName "Oranges"))
          , mkSigned alice "ev4" (ClosePurchase (PurchaseId "pur1"))
          ]
      case Map.lookup (PurchaseId "pur1") st.purchases of
        Nothing -> shouldEqual "found" "not found"
        Just ps -> ps.phase `shouldEqual` Closed

    it "fails a purchase and refunds commitments" do
      let
        st = replaySignedEvents
          [ mkSigned alice "ev1" (VoteRegisterMember (MemberName "Alice"))
          , mkSigned alice "ev2" (VoteElectReferente alice)
          , mkSigned alice "ev3" (VoteElectCassiere alice)
          , mkSigned alice "ev4" (Deposit alice (Cents 1000))
          , mkSigned alice "pur1" (OpenPurchase (PurchaseName "Oranges"))
          , mkSigned alice "ev5" (Commit alice (Cents 400) (PurchaseId "pur1"))
          , mkSigned alice "ev6" (FailPurchase (PurchaseId "pur1"))
          ]
      -- 1000 - 400 (commit) + 400 (refund) = 1000
      Map.lookup alice st.balances `shouldEqual` Just (Cents 1000)
      case Map.lookup (PurchaseId "pur1") st.purchases of
        Nothing -> shouldEqual "found" "not found"
        Just ps -> ps.phase `shouldEqual` Failed
