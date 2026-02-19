module View.Dashboard
  ( dashboardComponent
  , Output(..)
  , Input
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Domain.State (GroupState, PurchasePhase(..), adminCount)
import Domain.Types (PurchaseId(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input =
  { groupState :: GroupState
  , groupId :: Maybe String
  }

data Action
  = Receive Input
  | ClickPurchase PurchaseId
  | ClickMembers
  | ClickWallet

data Output
  = NavigatePurchase PurchaseId
  | NavigateMembers
  | NavigateWallet

dashboardComponent :: forall q m. H.Component q Input Output m
dashboardComponent = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      }
  }

render :: forall m. Input -> H.ComponentHTML Action () m
render { groupState: gs, groupId } = HH.div [ HP.class_ (HH.ClassName "dashboard") ]
  [ HH.h2_ [ HH.text "Dashboard" ]
  , case groupId of
      Just gid -> HH.p [ HP.class_ (HH.ClassName "group-id") ]
        [ HH.text ("Group: " <> gid) ]
      Nothing -> HH.text ""
  , HH.div [ HP.class_ (HH.ClassName "stats") ]
      [ stat "Members" (show (Map.size gs.members))
      , stat "Admins" (show (adminCount gs))
      , stat "Purchases" (show (Map.size gs.purchases))
      ]
  , HH.div [ HP.class_ (HH.ClassName "nav-buttons") ]
      [ HH.button [ HE.onClick (const ClickMembers) ] [ HH.text "Members" ]
      , HH.button [ HE.onClick (const ClickWallet) ] [ HH.text "Wallet" ]
      ]
  , HH.h3_ [ HH.text "Open Purchases" ]
  , HH.ul_ (map renderPurchase openPurchases)
  ]
  where
  openPurchases :: Array (Tuple PurchaseId _)
  openPurchases = Array.filter (\(Tuple _ ps) -> ps.phase == Open)
    (Map.toUnfoldable gs.purchases)

  renderPurchase (Tuple (PurchaseId pid) ps) = HH.li_
    [ HH.a
        [ HE.onClick (const (ClickPurchase (PurchaseId pid)))
        , HP.class_ (HH.ClassName "purchase-link")
        ]
        [ HH.text (show ps.name <> " (" <> show (Map.size ps.commitments) <> " commitments)") ]
    ]

  stat label value = HH.div [ HP.class_ (HH.ClassName "stat") ]
    [ HH.span [ HP.class_ (HH.ClassName "stat-value") ] [ HH.text value ]
    , HH.span [ HP.class_ (HH.ClassName "stat-label") ] [ HH.text label ]
    ]

handleAction :: forall m. Action -> H.HalogenM Input Action () Output m Unit
handleAction = case _ of
  Receive input -> H.put input
  ClickPurchase pid -> H.raise (NavigatePurchase pid)
  ClickMembers -> H.raise NavigateMembers
  ClickWallet -> H.raise NavigateWallet
