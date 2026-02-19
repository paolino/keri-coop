module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Keri.Cesr.RoundtripSpec as CesrSpec
import Test.Keri.Crypto.DigestSpec as DigestSpec
import Test.Keri.Event.InceptionSpec as InceptionSpec
import Test.Keri.KelSpec as KelSpec
import Test.Domain.StateSpec as StateSpec
import Test.Domain.ValidateSpec as ValidateSpec
import Test.Protocol.MessageSpec as MessageSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  CesrSpec.spec
  DigestSpec.spec
  InceptionSpec.spec
  KelSpec.spec
  StateSpec.spec
  ValidateSpec.spec
  MessageSpec.spec
