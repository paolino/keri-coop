module View.Members
  ( membersComponent
  , Output(..)
  , Input
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Domain.Event as DE
import Domain.State (GroupState)
import Domain.Types (MemberId, MemberName(..))
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
  , newMemberName :: String
  }

data Action
  = Receive Input
  | SetNewMemberName String
  | VoteRegister
  | VoteElectReferente MemberId
  | VoteElectCassiere MemberId
  | GoBack

data Output
  = SubmitDomainEvent DE.DomainEvent
  | NavigateBack

membersComponent :: forall q m. H.Component q Input Output m
membersComponent = H.mkComponent
  { initialState: \i -> { groupState: i.groupState, myId: i.myId, newMemberName: "" }
  , render
  , eval: H.mkEval H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render st = HH.div [ HP.class_ (HH.ClassName "members") ]
  [ HH.button [ HE.onClick (const GoBack) ] [ HH.text "Back" ]
  , HH.h2_ [ HH.text "Members" ]
  , HH.div [ HP.class_ (HH.ClassName "register-form") ]
      [ HH.input
          [ HP.placeholder "New member name"
          , HP.value st.newMemberName
          , HE.onValueInput SetNewMemberName
          ]
      , HH.button
          [ HE.onClick (const VoteRegister) ]
          [ HH.text "Vote Register" ]
      ]
  , HH.table_
      [ HH.thead_
          [ HH.tr_
              [ HH.th_ [ HH.text "Name" ]
              , HH.th_ [ HH.text "Roles" ]
              , HH.th_ [ HH.text "Actions" ]
              ]
          ]
      , HH.tbody_ (map renderMember members)
      ]
  ]
  where
  members :: Array (Tuple MemberId MemberName)
  members = Map.toUnfoldable st.groupState.members

  renderMember (Tuple mid (MemberName name)) = HH.tr_
    [ HH.td_ [ HH.text name ]
    , HH.td_ [ HH.text (roles mid) ]
    , HH.td_
        [ HH.button
            [ HE.onClick (const (VoteElectReferente mid)) ]
            [ HH.text "Referente" ]
        , HH.button
            [ HE.onClick (const (VoteElectCassiere mid)) ]
            [ HH.text "Cassiere" ]
        ]
    ]

  roles mid =
    let
      r = if Set.member mid st.groupState.referenti then "R" else ""
      c = if Set.member mid st.groupState.cassieri then "C" else ""
    in
      r <> c

handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive i -> H.modify_ _ { groupState = i.groupState, myId = i.myId }
  SetNewMemberName s -> H.modify_ _ { newMemberName = s }
  VoteRegister -> do
    st <- H.get
    when (st.newMemberName /= "") do
      H.raise (SubmitDomainEvent (DE.VoteRegisterMember (MemberName st.newMemberName)))
      H.modify_ _ { newMemberName = "" }
  VoteElectReferente mid -> H.raise (SubmitDomainEvent (DE.VoteElectReferente mid))
  VoteElectCassiere mid -> H.raise (SubmitDomainEvent (DE.VoteElectCassiere mid))
  GoBack -> H.raise NavigateBack
