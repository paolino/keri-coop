module Test.Main where

import Prelude

import Effect (Effect)
import Test.Keri.CesrSpec as CesrSpec
import Test.Keri.Cesr.DerivationCodeSpec as DerivationCodeSpec
import Test.Keri.Cesr.EncodeSpec as EncodeSpec
import Test.Keri.Cesr.DecodeSpec as DecodeSpec
import Test.Keri.Cesr.PrimitiveSpec as PrimitiveSpec
import Test.Keri.Crypto.Blake3Spec as Blake3Spec
import Test.Keri.Crypto.DigestSpec as DigestSpec
import Test.Keri.Crypto.Ed25519Spec as Ed25519Spec
import Test.Keri.Event.InceptionSpec as InceptionSpec
import Test.Keri.Event.RotationSpec as RotationSpec
import Test.Keri.Event.InteractionSpec as InteractionSpec
import Test.Keri.Event.SerializeSpec as SerializeSpec
import Test.Keri.KelSpec as KelSpec
import Test.Keri.KeyStateSpec as KeyStateSpec
import Test.Keri.KeyState.PreRotationSpec as PreRotationSpec
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter (consoleReporter)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  CesrSpec.spec
  DerivationCodeSpec.spec
  EncodeSpec.spec
  DecodeSpec.spec
  PrimitiveSpec.spec
  Blake3Spec.spec
  DigestSpec.spec
  Ed25519Spec.spec
  InceptionSpec.spec
  RotationSpec.spec
  InteractionSpec.spec
  SerializeSpec.spec
  KelSpec.spec
  KeyStateSpec.spec
  PreRotationSpec.spec
