module Test.Keri.KelSpec where

import Prelude hiding (append)

import Data.Either (Either(..), isLeft)
import Effect.Class (liftEffect)
import Keri.Event (Event(..), eventDigest, eventPrefix)
import Keri.Event.Interaction (mkInteraction)
import Keri.Event.Rotation (mkRotation)
import Keri.Kel (emptyKel)
import Keri.Kel.Append (append)
import Keri.Kel.Replay (replay)
import Keri.KeyState (statePrefix, stateSequenceNumber)
import Keri.KeyState.PreRotation (commitKey)
import Test.Keri.TestHelper (mkTestInception, mkTestKeyPair, signEvent)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "Kel" do
  describe "append" do
    it "appends inception to empty KEL" do
      { keyPair, event } ← mkTestInception
      let se = signEvent keyPair event
      case append emptyKel se of
        Left err → shouldEqual "Right" err
        Right _ → pure unit

    it "rejects event with tampered SAID" do
      { keyPair, event: icp } ← mkTestInception
      let se0 = signEvent keyPair icp
      case append emptyKel se0 of
        Left err → shouldEqual "Right" err
        Right kel → do
          let
            ixn = mkInteraction
              { prefix: eventPrefix icp
              , sequenceNumber: 1
              , priorDigest: eventDigest icp
              , anchors: []
              }
            tampered = case ixn of
              Interaction d → Interaction (d { priorDigest = "tampered" })
              other → other
            tamperedSe = { event: tampered, signatures: [] }
          case append kel tamperedSe of
            Left _ → pure unit
            Right _ → shouldEqual "Left" "Right"

    it "rejects non-inception as first event" do
      { event: icp } ← mkTestInception
      { keyPair: kp2 } ← mkTestInception
      let
        ixn = mkInteraction
          { prefix: eventPrefix icp
          , sequenceNumber: 1
          , priorDigest: eventDigest icp
          , anchors: []
          }
        se = signEvent kp2 ixn
      case append emptyKel se of
        Left _ → pure unit
        Right _ → shouldEqual "Left" "Right"

    it "appends interaction after inception" do
      { keyPair, event: icp } ← mkTestInception
      let se0 = signEvent keyPair icp
      case append emptyKel se0 of
        Left err → shouldEqual "Right" err
        Right kel → do
          let
            ixn = mkInteraction
              { prefix: eventPrefix icp
              , sequenceNumber: 1
              , priorDigest: eventDigest icp
              , anchors: []
              }
            se1 = signEvent keyPair ixn
          case append kel se1 of
            Left err → shouldEqual "Right" err
            Right _ → pure unit

    it "rejects wrong sequence" do
      { keyPair, event: icp } ← mkTestInception
      let se0 = signEvent keyPair icp
      case append emptyKel se0 of
        Left err → shouldEqual "Right" err
        Right kel → do
          let
            ixn = mkInteraction
              { prefix: eventPrefix icp
              , sequenceNumber: 5
              , priorDigest: eventDigest icp
              , anchors: []
              }
            se1 = signEvent keyPair ixn
          case append kel se1 of
            Left _ → pure unit
            Right _ → shouldEqual "Left" "Right"

  describe "replay" do
    it "replays inception to initial state" do
      { keyPair, event: icp } ← mkTestInception
      let se0 = signEvent keyPair icp
      case append emptyKel se0 of
        Left err → shouldEqual "Right" err
        Right kel → case replay kel of
          Left err → shouldEqual "Right" err
          Right ks → do
            statePrefix ks `shouldEqual` eventPrefix icp
            stateSequenceNumber ks `shouldEqual` 0

    it "replays inception + interaction" do
      { keyPair, event: icp } ← mkTestInception
      let se0 = signEvent keyPair icp
      case append emptyKel se0 of
        Left err → shouldEqual "Right" err
        Right kel → do
          let
            ixn = mkInteraction
              { prefix: eventPrefix icp
              , sequenceNumber: 1
              , priorDigest: eventDigest icp
              , anchors: []
              }
            se1 = signEvent keyPair ixn
          case append kel se1 of
            Left err → shouldEqual "Right" err
            Right kel2 → case replay kel2 of
              Left err → shouldEqual "Right" err
              Right ks → stateSequenceNumber ks `shouldEqual` 1

    it "replays inception + rotation" do
      { keyPair, nextKeyPair, event: icp } ← mkTestInception
      let se0 = signEvent keyPair icp
      case append emptyKel se0 of
        Left err → shouldEqual "Right" err
        Right kel → do
          newNext ← liftEffect mkTestKeyPair
          let
            commitment = case commitKey newNext.cesrPubKey of
              Right c → c
              Left _ → ""
            rot = mkRotation
              { prefix: eventPrefix icp
              , sequenceNumber: 1
              , priorDigest: eventDigest icp
              , keys: [ nextKeyPair.cesrPubKey ]
              , signingThreshold: 1
              , nextKeys: [ commitment ]
              , nextThreshold: 1
              , config: []
              , anchors: []
              }
            se1 = signEvent keyPair rot
          case append kel se1 of
            Left err → shouldEqual "Right" err
            Right kel2 → case replay kel2 of
              Left err → shouldEqual "Right" err
              Right ks → stateSequenceNumber ks `shouldEqual` 1

    it "fails on empty KEL" do
      replay emptyKel `shouldSatisfy` isLeft
