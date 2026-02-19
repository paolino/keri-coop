module View.Purchase
  ( purchaseComponent
  , Output(..)
  , Input
  ) where

import Prelude

import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Domain.Event (DomainEvent(..))
import Domain.State (GroupState, PurchasePhase(..), CommitmentStatus(..), quorum)
import Domain.Types (Cents(..), MemberId, MemberName(..), PurchaseId, PurchaseName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input =
  { groupState :: GroupState
  , purchaseId :: PurchaseId
  , myId :: Maybe MemberId
  }

type State =
  { groupState :: GroupState
  , purchaseId :: PurchaseId
  , myId :: Maybe MemberId
  , commitAmount :: String
  }

data Action
  = Receive Input
  | SetCommitAmount String
  | SubmitCommit
  | Approve MemberId
  | Reject MemberId
  | VoteClose
  | VoteFail
  | Close
  | Fail
  | GoBack

data Output
  = SubmitDomainEvent DomainEvent
  | NavigateBack

purchaseComponent :: forall q m. H.Component q Input Output m
purchaseComponent = H.mkComponent
  { initialState: \i ->
      { groupState: i.groupState
      , purchaseId: i.purchaseId
      , myId: i.myId
      , commitAmount: ""
      }
  , render
  , eval: H.mkEval H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render st = HH.div [ HP.class_ (HH.ClassName "purchase") ]
  case Map.lookup st.purchaseId st.groupState.purchases of
    Nothing -> [ HH.text "Purchase not found" ]
    Just ps ->
      [ HH.button [ HE.onClick (const GoBack) ] [ HH.text "Back" ]
      , HH.h2_ [ HH.text (nameStr ps.name) ]
      , HH.p_ [ HH.text ("Phase: " <> phaseStr ps.phase) ]
      , HH.p_ [ HH.text ("Close votes: " <> show (Set.size ps.closeVotes) <> "/" <> show (quorum st.groupState)) ]
      , HH.p_ [ HH.text ("Fail votes: " <> show (Set.size ps.failVotes) <> "/" <> show (quorum st.groupState)) ]
      , HH.h3_ [ HH.text "Commitments" ]
      , HH.table_
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ [ HH.text "Member" ]
                  , HH.th_ [ HH.text "Amount" ]
                  , HH.th_ [ HH.text "Status" ]
                  , HH.th_ [ HH.text "Actions" ]
                  ]
              ]
          , HH.tbody_ (map renderCommitment (Map.toUnfoldable ps.commitments))
          ]
      , if ps.phase == Open then
          HH.div [ HP.class_ (HH.ClassName "form") ]
            [ HH.h3_ [ HH.text "Commit" ]
            , HH.input [ HP.placeholder "Amount (cents)", HP.value st.commitAmount, HE.onValueInput SetCommitAmount ]
            , HH.button [ HE.onClick (const SubmitCommit) ] [ HH.text "Commit" ]
            , HH.h3_ [ HH.text "Admin actions" ]
            , HH.button [ HE.onClick (const VoteClose) ] [ HH.text "Vote Close" ]
            , HH.button [ HE.onClick (const VoteFail) ] [ HH.text "Vote Fail" ]
            , HH.button [ HE.onClick (const Close) ] [ HH.text "Close" ]
            , HH.button [ HE.onClick (const Fail) ] [ HH.text "Fail" ]
            ]
        else HH.text ""
      ]
  where
  renderCommitment (Tuple mid { amount: Cents cents, status }) =
    let
      memberName = case Map.lookup mid st.groupState.members of
        Just (MemberName n) -> n
        Nothing -> show mid
    in
      HH.tr_
        [ HH.td_ [ HH.text memberName ]
        , HH.td_ [ HH.text (show cents) ]
        , HH.td_ [ HH.text (statusStr status) ]
        , HH.td_
            [ HH.button [ HE.onClick (const (Approve mid)) ] [ HH.text "Approve" ]
            , HH.button [ HE.onClick (const (Reject mid)) ] [ HH.text "Reject" ]
            ]
        ]

  nameStr (PurchaseName n) = n
  phaseStr Open = "Open"
  phaseStr Closed = "Closed"
  phaseStr Failed = "Failed"
  statusStr Pending = "Pending"
  statusStr Approved = "Approved"
  statusStr Rejected = "Rejected"

handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive i -> H.modify_ _ { groupState = i.groupState, purchaseId = i.purchaseId, myId = i.myId }
  SetCommitAmount s -> H.modify_ _ { commitAmount = s }
  SubmitCommit -> do
    st <- H.get
    case st.myId, Int.fromString st.commitAmount of
      Just mid, Just cents -> do
        H.raise (SubmitDomainEvent (Commit mid (Cents cents) st.purchaseId))
        H.modify_ _ { commitAmount = "" }
      _, _ -> pure unit
  Approve mid -> do
    st <- H.get
    H.raise (SubmitDomainEvent (ApproveCommitment mid st.purchaseId))
  Reject mid -> do
    st <- H.get
    H.raise (SubmitDomainEvent (RejectCommitment mid st.purchaseId))
  VoteClose -> do
    st <- H.get
    H.raise (SubmitDomainEvent (VoteClosePurchase st.purchaseId))
  VoteFail -> do
    st <- H.get
    H.raise (SubmitDomainEvent (VoteFailPurchase st.purchaseId))
  Close -> do
    st <- H.get
    H.raise (SubmitDomainEvent (ClosePurchase st.purchaseId))
  Fail -> do
    st <- H.get
    H.raise (SubmitDomainEvent (FailPurchase st.purchaseId))
  GoBack -> H.raise NavigateBack
