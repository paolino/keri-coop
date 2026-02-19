module View.Wallet
  ( walletComponent
  , Output(..)
  , Input
  ) where

import Prelude

import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Domain.Event (DomainEvent(..))
import Domain.State (GroupState)
import Domain.Types (AID(..), Cents(..), MemberId(..), MemberName(..), Reason(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input =
  { groupState :: GroupState
  , myId :: Maybe MemberId
  }

type State =
  { groupState :: GroupState
  , myId :: Maybe MemberId
  , depositMember :: String
  , depositAmount :: String
  , withdrawMember :: String
  , withdrawAmount :: String
  , withdrawReason :: String
  }

data Action
  = Receive Input
  | SetDepositMember String
  | SetDepositAmount String
  | SetWithdrawMember String
  | SetWithdrawAmount String
  | SetWithdrawReason String
  | SubmitDeposit
  | SubmitWithdraw
  | GoBack

data Output
  = SubmitDomainEvent DomainEvent
  | NavigateBack

walletComponent :: forall q m. H.Component q Input Output m
walletComponent = H.mkComponent
  { initialState: \i ->
      { groupState: i.groupState
      , myId: i.myId
      , depositMember: ""
      , depositAmount: ""
      , withdrawMember: ""
      , withdrawAmount: ""
      , withdrawReason: ""
      }
  , render
  , eval: H.mkEval H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render st = HH.div [ HP.class_ (HH.ClassName "wallet") ]
  [ HH.button [ HE.onClick (const GoBack) ] [ HH.text "Back" ]
  , HH.h2_ [ HH.text "Wallet" ]
  , HH.h3_ [ HH.text "Balances" ]
  , HH.table_
      [ HH.thead_
          [ HH.tr_
              [ HH.th_ [ HH.text "Member" ]
              , HH.th_ [ HH.text "Balance" ]
              ]
          ]
      , HH.tbody_ (map renderBalance balances)
      ]
  , HH.h3_ [ HH.text "Deposit" ]
  , HH.div [ HP.class_ (HH.ClassName "form") ]
      [ HH.input [ HP.placeholder "Member AID", HP.value st.depositMember, HE.onValueInput SetDepositMember ]
      , HH.input [ HP.placeholder "Amount (cents)", HP.value st.depositAmount, HE.onValueInput SetDepositAmount ]
      , HH.button [ HE.onClick (const SubmitDeposit) ] [ HH.text "Deposit" ]
      ]
  , HH.h3_ [ HH.text "Withdraw" ]
  , HH.div [ HP.class_ (HH.ClassName "form") ]
      [ HH.input [ HP.placeholder "Member AID", HP.value st.withdrawMember, HE.onValueInput SetWithdrawMember ]
      , HH.input [ HP.placeholder "Amount (cents)", HP.value st.withdrawAmount, HE.onValueInput SetWithdrawAmount ]
      , HH.input [ HP.placeholder "Reason", HP.value st.withdrawReason, HE.onValueInput SetWithdrawReason ]
      , HH.button [ HE.onClick (const SubmitWithdraw) ] [ HH.text "Withdraw" ]
      ]
  ]
  where
  balances :: Array (Tuple MemberId Cents)
  balances = Map.toUnfoldable st.groupState.balances

  renderBalance (Tuple mid (Cents cents)) =
    let
      name = case Map.lookup mid st.groupState.members of
        Just (MemberName n) -> n
        Nothing -> show mid
    in
      HH.tr_
        [ HH.td_ [ HH.text name ]
        , HH.td_ [ HH.text (show cents) ]
        ]

handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive i -> H.modify_ _ { groupState = i.groupState, myId = i.myId }
  SetDepositMember s -> H.modify_ _ { depositMember = s }
  SetDepositAmount s -> H.modify_ _ { depositAmount = s }
  SetWithdrawMember s -> H.modify_ _ { withdrawMember = s }
  SetWithdrawAmount s -> H.modify_ _ { withdrawAmount = s }
  SetWithdrawReason s -> H.modify_ _ { withdrawReason = s }
  SubmitDeposit -> do
    st <- H.get
    case Int.fromString st.depositAmount of
      Just cents -> do
        let mid = parseMemberId st.depositMember
        H.raise (SubmitDomainEvent (Deposit mid (Cents cents)))
        H.modify_ _ { depositMember = "", depositAmount = "" }
      Nothing -> pure unit
  SubmitWithdraw -> do
    st <- H.get
    case Int.fromString st.withdrawAmount of
      Just cents -> do
        let mid = parseMemberId st.withdrawMember
        H.raise (SubmitDomainEvent (Withdraw mid (Cents cents) (Reason st.withdrawReason)))
        H.modify_ _ { withdrawMember = "", withdrawAmount = "", withdrawReason = "" }
      Nothing -> pure unit
  GoBack -> H.raise NavigateBack

parseMemberId :: String -> MemberId
parseMemberId s = MemberId (AID s)
