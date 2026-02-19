module View.Types
  ( Screen(..)
  , Identity
  , AppState
  , AppAction(..)
  , initialState
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Domain.State (GroupState)
import Domain.State as DS
import Domain.Types (MemberId, PurchaseId)
import FFI.TweetNaCl (KeyPair)
import FFI.WebSocket (WebSocket)

-- | Identity: keypair + AID prefix
type Identity =
  { keyPair :: KeyPair
  , prefix :: String
  , memberId :: MemberId
  }

-- | Navigation screens
data Screen
  = IdentityScreen
  | DashboardScreen
  | MembersScreen
  | WalletScreen
  | PurchaseScreen PurchaseId

derive instance eqScreen :: Eq Screen

-- | Root application state
type AppState =
  { screen :: Screen
  , identity :: Maybe Identity
  , groupId :: Maybe String
  , groupState :: GroupState
  , sequenceNumber :: Int
  , ws :: Maybe WebSocket
  , error :: Maybe String
  , inputValue :: String
  , inputAmount :: String
  }

initialState :: AppState
initialState =
  { screen: IdentityScreen
  , identity: Nothing
  , groupId: Nothing
  , groupState: DS.emptyState
  , sequenceNumber: 0
  , ws: Nothing
  , error: Nothing
  , inputValue: ""
  , inputAmount: ""
  }

-- | Application actions
data AppAction
  = Initialize
  | Navigate Screen
  | SetInputValue String
  | SetInputAmount String
  | GenerateIdentity
  | IdentityLoaded Identity
  | CreateGroup
  | JoinGroup String
  | GroupJoined String
  | SubmitEvent
  | EventAppended
  | EventReceived String
  | SetError String
  | ClearError
