module Test.Keri.KeyStateSpec where

import Prelude

import Data.Either (Either(..), isLeft)
import Effect.Class (liftEffect)
import Keri.Event (Event(..))
import Keri.Event.Interaction (mkInteraction)
import Keri.Event.Rotation (mkRotation)
import Keri.KeyState
  ( applyEvent
  , initialState
  , stateKeys
  , statePrefix
  , stateSequenceNumber
  , stateSigningThreshold
  )
import Keri.KeyState.PreRotation (commitKey)
import Test.Keri.TestHelper (mkTestInception, mkTestKeyPair)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "KeyState" do
  describe "initialState" do
    it "sets prefix from inception" do
      { event } ← mkTestInception
      case event of
        Inception d → do
          let ks = initialState d
          statePrefix ks `shouldEqual` d.prefix
        _ → pure unit

    it "sets sequence number to 0" do
      { event } ← mkTestInception
      case event of
        Inception d →
          stateSequenceNumber (initialState d) `shouldEqual` 0
        _ → pure unit

    it "sets keys from inception" do
      { event } ← mkTestInception
      case event of
        Inception d → do
          let ks = initialState d
          stateKeys ks `shouldEqual` d.keys
        _ → pure unit

    it "sets signing threshold" do
      { event } ← mkTestInception
      case event of
        Inception d →
          stateSigningThreshold (initialState d) `shouldEqual` d.signingThreshold
        _ → pure unit

  describe "applyEvent" do
    it "rejects inception on existing state" do
      { event } ← mkTestInception
      case event of
        Inception d → do
          let ks = initialState d
          applyEvent ks event `shouldSatisfy` isLeft
        _ → pure unit

    it "applies interaction with correct sequence" do
      { event: icp } ← mkTestInception
      case icp of
        Inception d → do
          let
            ks = initialState d
            ixn = mkInteraction
              { prefix: d.prefix
              , sequenceNumber: 1
              , priorDigest: d.digest
              , anchors: []
              }
          case applyEvent ks ixn of
            Left err → shouldEqual "Right" err
            Right ks' → stateSequenceNumber ks' `shouldEqual` 1
        _ → pure unit

    it "rejects wrong sequence number" do
      { event: icp } ← mkTestInception
      case icp of
        Inception d → do
          let
            ks = initialState d
            ixn = mkInteraction
              { prefix: d.prefix
              , sequenceNumber: 5
              , priorDigest: d.digest
              , anchors: []
              }
          applyEvent ks ixn `shouldSatisfy` isLeft
        _ → pure unit

    it "rejects wrong prefix" do
      { event: icp } ← mkTestInception
      case icp of
        Inception d → do
          let
            ks = initialState d
            ixn = mkInteraction
              { prefix: "wrong-prefix"
              , sequenceNumber: 1
              , priorDigest: d.digest
              , anchors: []
              }
          applyEvent ks ixn `shouldSatisfy` isLeft
        _ → pure unit

    it "rejects wrong prior digest" do
      { event: icp } ← mkTestInception
      case icp of
        Inception d → do
          let
            ks = initialState d
            ixn = mkInteraction
              { prefix: d.prefix
              , sequenceNumber: 1
              , priorDigest: "wrong-digest"
              , anchors: []
              }
          applyEvent ks ixn `shouldSatisfy` isLeft
        _ → pure unit

    it "applies rotation with valid pre-rotation" do
      { event: icp, nextKeyPair } ← mkTestInception
      newNext ← liftEffect mkTestKeyPair
      case icp of
        Inception d → do
          let
            ks = initialState d
            commitment = case commitKey newNext.cesrPubKey of
              Right c → c
              Left _ → ""
            rot = mkRotation
              { prefix: d.prefix
              , sequenceNumber: 1
              , priorDigest: d.digest
              , keys: [ nextKeyPair.cesrPubKey ]
              , signingThreshold: 1
              , nextKeys: [ commitment ]
              , nextThreshold: 1
              , config: []
              , anchors: []
              }
          case applyEvent ks rot of
            Left err → shouldEqual "Right" err
            Right ks' → do
              stateSequenceNumber ks' `shouldEqual` 1
              stateKeys ks' `shouldEqual` [ nextKeyPair.cesrPubKey ]
        _ → pure unit
