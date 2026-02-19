module Test.Domain.ValidateSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Domain.Event (DomainEvent(..))
import Domain.State (GroupState, SignedEvent, replaySignedEvents)
import Domain.Types (AID(..), Cents(..), MemberId(..), MemberName(..), PurchaseId(..), PurchaseName(..), Reason(..))
import Domain.Validate (ValidationError(..), validateEvent)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

mkSigned :: MemberId -> String -> DomainEvent -> SignedEvent
mkSigned signer eventId event = { signer, eventId, event }

alice :: MemberId
alice = MemberId (AID "alice-aid")

bob :: MemberId
bob = MemberId (AID "bob-aid")

-- State with alice as referente + cassiere + registered member
withAlice :: GroupState
withAlice = replaySignedEvents
  [ mkSigned alice "ev1" (VoteRegisterMember (MemberName "Alice"))
  , mkSigned alice "ev2" (VoteElectReferente alice)
  , mkSigned alice "ev3" (VoteElectCassiere alice)
  ]

spec :: Spec Unit
spec = describe "Domain.Validate" do
  describe "governance" do
    it "allows admin to vote" do
      validateEvent alice (VoteRegisterMember (MemberName "Bob")) withAlice
        `shouldEqual` Nothing

    it "rejects non-admin vote" do
      validateEvent bob (VoteRegisterMember (MemberName "Carol")) withAlice
        `shouldEqual` Just (NotAnAdmin bob)

  describe "economics" do
    it "allows cassiere to deposit" do
      validateEvent alice (Deposit alice (Cents 100)) withAlice
        `shouldEqual` Nothing

    it "rejects non-cassiere deposit" do
      validateEvent bob (Deposit alice (Cents 100)) withAlice
        `shouldEqual` Just (NotACassiere bob)

    it "rejects withdrawal for missing member" do
      validateEvent alice (Withdraw bob (Cents 100) (Reason "test")) withAlice
        `shouldEqual` Just (MemberNotFound bob)

  describe "purchases" do
    it "allows referente to open purchase" do
      validateEvent alice (OpenPurchase (PurchaseName "Test")) withAlice
        `shouldEqual` Nothing

    it "rejects non-referente opening purchase" do
      validateEvent bob (OpenPurchase (PurchaseName "Test")) withAlice
        `shouldEqual` Just (NotAReferente bob)

    it "rejects commit to nonexistent purchase" do
      validateEvent alice (Commit alice (Cents 100) (PurchaseId "xxx")) withAlice
        `shouldEqual` Just (PurchaseNotFound (PurchaseId "xxx"))

    it "rejects vote close for nonexistent purchase" do
      validateEvent alice (VoteClosePurchase (PurchaseId "xxx")) withAlice
        `shouldEqual` Just (PurchaseNotFound (PurchaseId "xxx"))

    it "allows commit to open purchase with balance" do
      let
        st = replaySignedEvents
          [ mkSigned alice "ev1" (VoteRegisterMember (MemberName "Alice"))
          , mkSigned alice "ev2" (VoteElectReferente alice)
          , mkSigned alice "ev3" (VoteElectCassiere alice)
          , mkSigned alice "ev4" (Deposit alice (Cents 1000))
          , mkSigned alice "pur1" (OpenPurchase (PurchaseName "Oranges"))
          ]
      validateEvent alice (Commit alice (Cents 500) (PurchaseId "pur1")) st
        `shouldEqual` Nothing

    it "rejects commit with insufficient balance" do
      let
        st = replaySignedEvents
          [ mkSigned alice "ev1" (VoteRegisterMember (MemberName "Alice"))
          , mkSigned alice "ev2" (VoteElectReferente alice)
          , mkSigned alice "ev3" (VoteElectCassiere alice)
          , mkSigned alice "ev4" (Deposit alice (Cents 100))
          , mkSigned alice "pur1" (OpenPurchase (PurchaseName "Oranges"))
          ]
      validateEvent alice (Commit alice (Cents 500) (PurchaseId "pur1")) st
        `shouldEqual` Just (InsufficientBalance alice)
