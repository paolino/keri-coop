module View.App
  ( appComponent
  ) where

import Prelude

import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Domain.Event (DomainEvent(..), serializeDomainEvent)
import Domain.State as DS
import Domain.Types (PurchaseName(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Protocol.Client as Client
import Protocol.Message as Msg
import Type.Proxy (Proxy(..))
import View.Dashboard as Dashboard
import View.Identity as Identity
import View.Members as Members
import View.Purchase as Purchase
import View.Types (Identity, Screen(..))
import View.Wallet as Wallet

type State =
  { screen :: Screen
  , identity :: Maybe Identity
  , groupId :: Maybe String
  , groupState :: DS.GroupState
  , sequenceNumber :: Int
  , priorDigest :: String
  , newGroupInput :: String
  , newPurchaseInput :: String
  , error :: Maybe String
  }

data Action
  = HandleIdentity Identity
  | HandleDashboard Dashboard.Output
  | HandleMembers Members.Output
  | HandleWallet Wallet.Output
  | HandlePurchase Purchase.Output
  | SetNewGroupInput String
  | SetNewPurchaseInput String
  | CreateGroup
  | JoinGroup
  | OpenNewPurchase
  | SubmitEvent DomainEvent
  | Navigate Screen

type Slots =
  ( identity :: H.Slot (Const Void) Identity.Output Unit
  , dashboard :: H.Slot (Const Void) Dashboard.Output Unit
  , members :: H.Slot (Const Void) Members.Output Unit
  , wallet :: H.Slot (Const Void) Wallet.Output Unit
  , purchase :: H.Slot (Const Void) Purchase.Output Unit
  )

appComponent :: forall q i o m. MonadAff m => H.Component q i o m
appComponent = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
  }

initialState :: State
initialState =
  { screen: IdentityScreen
  , identity: Nothing
  , groupId: Nothing
  , groupState: DS.emptyState
  , sequenceNumber: 0
  , priorDigest: ""
  , newGroupInput: ""
  , newPurchaseInput: ""
  , error: Nothing
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render st = HH.div [ HP.class_ (HH.ClassName "app") ]
  [ header st
  , case st.error of
      Just err -> HH.div [ HP.class_ (HH.ClassName "error-bar") ] [ HH.text err ]
      Nothing -> HH.text ""
  , content st
  ]

header :: forall m. State -> H.ComponentHTML Action Slots m
header st = HH.nav [ HP.class_ (HH.ClassName "header") ]
  [ HH.h1_ [ HH.text "keri-coop" ]
  , case st.identity of
      Just ident -> HH.span [ HP.class_ (HH.ClassName "user-id") ]
        [ HH.text (take8 ident.prefix) ]
      Nothing -> HH.text ""
  ]
  where
  take8 s = if String.length s > 8 then String.take 8 s <> "..." else s

content :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
content st = case st.screen of
  IdentityScreen ->
    HH.slot (Proxy :: _ "identity") unit Identity.identityComponent unit HandleIdentity

  DashboardScreen -> case st.groupId of
    Nothing -> groupSetup st
    Just _ ->
      HH.div_
        [ HH.slot (Proxy :: _ "dashboard") unit Dashboard.dashboardComponent
            { groupState: st.groupState, groupId: st.groupId }
            HandleDashboard
        , HH.div [ HP.class_ (HH.ClassName "form") ]
            [ HH.input
                [ HP.placeholder "Purchase name"
                , HP.value st.newPurchaseInput
                , HE.onValueInput SetNewPurchaseInput
                ]
            , HH.button [ HE.onClick (const OpenNewPurchase) ] [ HH.text "Open Purchase" ]
            ]
        ]

  MembersScreen ->
    HH.slot (Proxy :: _ "members") unit Members.membersComponent
      { groupState: st.groupState, myId: map _.memberId st.identity }
      HandleMembers

  WalletScreen ->
    HH.slot (Proxy :: _ "wallet") unit Wallet.walletComponent
      { groupState: st.groupState, myId: map _.memberId st.identity }
      HandleWallet

  PurchaseScreen pid ->
    HH.slot (Proxy :: _ "purchase") unit Purchase.purchaseComponent
      { groupState: st.groupState, purchaseId: pid, myId: map _.memberId st.identity }
      HandlePurchase

groupSetup :: forall m. State -> H.ComponentHTML Action Slots m
groupSetup st = HH.div [ HP.class_ (HH.ClassName "group-setup") ]
  [ HH.h2_ [ HH.text "Join or Create a Group" ]
  , HH.div [ HP.class_ (HH.ClassName "form") ]
      [ HH.input
          [ HP.placeholder "Group ID"
          , HP.value st.newGroupInput
          , HE.onValueInput SetNewGroupInput
          ]
      , HH.button [ HE.onClick (const JoinGroup) ] [ HH.text "Join" ]
      , HH.button [ HE.onClick (const CreateGroup) ] [ HH.text "Create New" ]
      ]
  ]

baseUrl :: String
baseUrl = ""

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  HandleIdentity ident -> do
    H.modify_ _ { identity = Just ident, screen = DashboardScreen }

  HandleDashboard output -> case output of
    Dashboard.NavigatePurchase pid -> H.modify_ _ { screen = PurchaseScreen pid }
    Dashboard.NavigateMembers -> H.modify_ _ { screen = MembersScreen }
    Dashboard.NavigateWallet -> H.modify_ _ { screen = WalletScreen }

  HandleMembers output -> case output of
    Members.SubmitDomainEvent ev -> handleAction (SubmitEvent ev)
    Members.NavigateBack -> H.modify_ _ { screen = DashboardScreen }

  HandleWallet output -> case output of
    Wallet.SubmitDomainEvent ev -> handleAction (SubmitEvent ev)
    Wallet.NavigateBack -> H.modify_ _ { screen = DashboardScreen }

  HandlePurchase output -> case output of
    Purchase.SubmitDomainEvent ev -> handleAction (SubmitEvent ev)
    Purchase.NavigateBack -> H.modify_ _ { screen = DashboardScreen }

  SetNewGroupInput s -> H.modify_ _ { newGroupInput = s }
  SetNewPurchaseInput s -> H.modify_ _ { newPurchaseInput = s }

  CreateGroup -> do
    H.modify_ _ { error = Nothing }
    result <- liftAff $ Client.createGroup baseUrl
    H.modify_ _ { groupId = Just result, groupState = DS.emptyState }

  JoinGroup -> do
    st <- H.get
    when (st.newGroupInput /= "") do
      H.modify_ _ { groupId = Just st.newGroupInput, newGroupInput = "", error = Nothing }

  OpenNewPurchase -> do
    st <- H.get
    when (st.newPurchaseInput /= "") do
      handleAction (SubmitEvent (OpenPurchase (PurchaseName st.newPurchaseInput)))
      H.modify_ _ { newPurchaseInput = "" }

  SubmitEvent ev -> do
    st <- H.get
    case st.identity, st.groupId of
      Just ident, Just _ -> do
        case
          Msg.mkGroupMessage
            { prefix: ident.prefix
            , sequenceNumber: st.sequenceNumber
            , priorDigest: st.priorDigest
            , secretKey: ident.keyPair
            , keyIndex: 0
            }
            ev
          of
          Left err ->
            H.modify_ _ { error = Just ("Sign failed: " <> err) }
          Right msg -> do
            let
              payload = serializeDomainEvent ev
              signed = Msg.extractSignedEvent msg
              newState = DS.applySignedEvent signed st.groupState
            H.modify_ _
              { groupState = newState
              , sequenceNumber = st.sequenceNumber + 1
              , error = Nothing
              }
            -- Send to server (fire-and-forget, log errors)
            case st.groupId of
              Just groupId -> liftAff do
                _ <- Client.appendEvent baseUrl groupId st.sequenceNumber payload
                pure unit
              Nothing -> pure unit
      _, _ ->
        H.modify_ _ { error = Just "No identity or group" }

  Navigate screen -> H.modify_ _ { screen = screen }
