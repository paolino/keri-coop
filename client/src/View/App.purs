module View.App
  ( appComponent
  ) where

import Prelude

import Data.Argonaut.Decode (decodeJson, printJsonDecodeError, (.:))
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String as String
import Domain.Event (DomainEvent(..))
import Domain.State as DS
import Domain.Types (PurchaseName(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import FFI.WebSocket as WS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Keri.Event (eventDigest)
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
  , keriSeqNo :: Int
  , priorDigest :: String
  , serverSeqNo :: Int
  , ws :: Maybe WS.WebSocket
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
  | EventReceived String

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
  , keriSeqNo: 0
  , priorDigest: ""
  , serverSeqNo: 0
  , ws: Nothing
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
    groupId <- liftAff $ Client.createGroup baseUrl
    H.modify_ _
      { groupId = Just groupId
      , groupState = DS.emptyState
      , serverSeqNo = 0
      }
    startWebSocket groupId

  JoinGroup -> do
    st <- H.get
    when (st.newGroupInput /= "") do
      let groupId = st.newGroupInput
      H.modify_ _
        { groupId = Just groupId
        , newGroupInput = ""
        , error = Nothing
        , groupState = DS.emptyState
        , serverSeqNo = 0
        }
      -- Fetch all existing events and replay them
      events <- liftAff $ Client.fetchEvents baseUrl groupId (-1)
      let
        applyFetched acc ev = case Msg.deserializeSignedEvent ev.payload of
          Left _ -> acc { lastSeq = ev.seq }
          Right signed -> acc
            { groupState = DS.applySignedEvent signed acc.groupState
            , lastSeq = ev.seq
            }
        result = foldl applyFetched
          { groupState: DS.emptyState, lastSeq: -1 }
          events
      H.modify_ _
        { groupState = result.groupState
        , serverSeqNo = result.lastSeq + 1
        }
      startWebSocket groupId

  OpenNewPurchase -> do
    st <- H.get
    when (st.newPurchaseInput /= "") do
      handleAction (SubmitEvent (OpenPurchase (PurchaseName st.newPurchaseInput)))
      H.modify_ _ { newPurchaseInput = "" }

  SubmitEvent ev -> do
    st <- H.get
    case st.identity, st.groupId of
      Just ident, Just groupId -> do
        case
          Msg.mkGroupMessage
            { prefix: ident.prefix
            , sequenceNumber: st.keriSeqNo
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
              signed = Msg.extractSignedEvent msg
              payload = Msg.serializeSignedEvent signed
              newState = DS.applySignedEvent signed st.groupState
              newDigest = eventDigest msg.keriEvent.event
            H.modify_ _
              { groupState = newState
              , keriSeqNo = st.keriSeqNo + 1
              , priorDigest = newDigest
              , error = Nothing
              }
            serverSeq <- liftAff $
              Client.appendEvent baseUrl groupId st.serverSeqNo payload
            H.modify_ _ { serverSeqNo = serverSeq + 1 }
      _, _ ->
        H.modify_ _ { error = Just "No identity or group" }

  Navigate screen -> H.modify_ _ { screen = screen }

  EventReceived msgStr -> do
    st <- H.get
    case st.identity of
      Nothing -> pure unit
      Just ident ->
        case parseWsMessage msgStr of
          Left err ->
            H.modify_ _ { error = Just ("WS parse error: " <> err) }
          Right { seq: serverSeq, payload } ->
            case Msg.deserializeSignedEvent payload of
              Left err ->
                H.modify_ _ { error = Just ("Deserialize error: " <> err) }
              Right signed ->
                if signed.signer == ident.memberId then
                  H.modify_ _ { serverSeqNo = serverSeq + 1 }
                else do
                  let newState = DS.applySignedEvent signed st.groupState
                  H.modify_ _ { groupState = newState, serverSeqNo = serverSeq + 1 }

-- | Open a WebSocket subscription for the current group.
startWebSocket
  :: forall o m
   . MonadAff m
  => String
  -> H.HalogenM State Action Slots o m Unit
startWebSocket groupId = do
  wsUrl <- liftEffect Client.getWsUrl
  { emitter, listener } <- liftEffect HS.create
  void $ H.subscribe emitter
  ws <- liftEffect $ Client.subscribeGroup wsUrl groupId \msg ->
    HS.notify listener (EventReceived msg)
  H.modify_ _ { ws = Just ws }

-- | Parse a WebSocket event message: @{ "seq": N, "payload": "..." }@
parseWsMessage :: String -> Either String { seq :: Int, payload :: String }
parseWsMessage s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError do
    obj <- decodeJson json
    seq <- obj .: "seq"
    payload <- obj .: "payload"
    pure { seq, payload }
