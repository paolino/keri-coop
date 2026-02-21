module Test.Keri.Event.SerializeSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Keri.Event (Event(..))
import Keri.Event.Serialize (serializeEvent)
import Test.Keri.TestHelper (mkTestInception)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)

spec :: Spec Unit
spec = describe "Event.Serialize" do
  describe "serializeEvent" do
    it "inception contains icp type" do
      { event } ← mkTestInception
      let json = serializeEvent event
      json `shouldSatisfy` String.contains (String.Pattern "\"t\":\"icp\"")

    it "inception contains version" do
      { event } ← mkTestInception
      let json = serializeEvent event
      json `shouldSatisfy` String.contains (String.Pattern "KERI10JSON")

    it "inception starts with {" do
      { event } ← mkTestInception
      String.take 1 (serializeEvent event) `shouldSatisfy` (_ == "{")

    it "inception ends with }" do
      { event } ← mkTestInception
      let
        json = serializeEvent event
        lastChar = String.drop (String.length json - 1) json
      lastChar `shouldSatisfy` (_ == "}")

    it "inception has deterministic output" do
      { event } ← mkTestInception
      serializeEvent event `shouldSatisfy` (_ == serializeEvent event)

    it "rotation ba comes before br" do
      { event } ← mkTestInception
      case event of
        Inception d → do
          let
            rotData =
              { version: d.version
              , digest: d.digest
              , prefix: d.prefix
              , sequenceNumber: 1
              , priorDigest: d.digest
              , signingThreshold: 1
              , keys: d.keys
              , nextThreshold: 1
              , nextKeys: d.nextKeys
              , witnessThreshold: 0
              , witnessesRemoved: []
              , witnessesAdded: []
              , config: []
              , anchors: []
              }
            json = serializeEvent (Rotation rotData)
            baPos = String.indexOf (String.Pattern "\"ba\"") json
            brPos = String.indexOf (String.Pattern "\"br\"") json
          case baPos, brPos of
            Just ba, Just br → ba `shouldSatisfy` (_ < br)
            _, _ → shouldSatisfy "both found" (const false)
        _ → pure unit
