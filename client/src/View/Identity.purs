module View.Identity
  ( identityComponent
  , Output
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Domain.Types (AID(..), MemberId(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import FFI.Storage as Storage
import FFI.TweetNaCl as NaCl
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode as Cesr
import Keri.Cesr.Primitive (mkPrimitive)
import Data.Either (Either(..))
import View.Types (Identity)

type State =
  { identity :: Maybe Identity
  , error :: Maybe String
  }

data Action
  = Init
  | Generate
  | LoadExisting

type Output = Identity

identityComponent :: forall q m. MonadAff m => H.Component q Unit Output m
identityComponent = H.mkComponent
  { initialState: const { identity: Nothing, error: Nothing }
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render st = HH.div [ HP.class_ (HH.ClassName "identity-screen") ]
  case st.identity of
    Just ident ->
      [ HH.h2_ [ HH.text "Your Identity" ]
      , HH.p [ HP.class_ (HH.ClassName "aid") ]
          [ HH.text ident.prefix ]
      , HH.p_ [ HH.text "Identity loaded. Ready to join a group." ]
      ]
    Nothing ->
      [ HH.h2_ [ HH.text "Welcome to keri-coop" ]
      , HH.p_ [ HH.text "Generate a new KERI identity to get started." ]
      , HH.button
          [ HE.onClick (const Generate)
          , HP.class_ (HH.ClassName "btn-primary")
          ]
          [ HH.text "Generate Identity" ]
      , case st.error of
          Just err -> HH.p [ HP.class_ (HH.ClassName "error") ] [ HH.text err ]
          Nothing -> HH.text ""
      ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Init -> do
    mPrefix <- liftEffect $ Storage.getItem "keri-coop-prefix"
    case mPrefix of
      Just _ -> handleAction LoadExisting
      Nothing -> pure unit

  LoadExisting -> do
    mPrefix <- liftEffect $ Storage.getItem "keri-coop-prefix"
    mSecret <- liftEffect $ Storage.getItem "keri-coop-secret"
    case mPrefix, mSecret of
      Just prefix, Just _ -> do
        kp <- liftEffect NaCl.generateKeyPair
        let
          ident =
            { keyPair: kp
            , prefix
            , memberId: MemberId (AID prefix)
            }
        H.modify_ _ { identity = Just ident }
        H.raise ident
      _, _ -> pure unit

  Generate -> do
    kp <- liftEffect NaCl.generateKeyPair
    case mkPrimitive Ed25519PubKey kp.publicKey of
      Left err -> H.modify_ _ { error = Just err }
      Right prim -> do
        let
          prefix = Cesr.encode prim
          ident =
            { keyPair: kp
            , prefix
            , memberId: MemberId (AID prefix)
            }
        liftEffect do
          Storage.setItem "keri-coop-prefix" prefix
        H.modify_ _ { identity = Just ident, error = Nothing }
        H.raise ident
