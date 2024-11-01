{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Binary.CddlSpec (spec) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Core
import Test.Cardano.Ledger.Allegra.Binary.Cddl (readAllegraCddlFiles)
import Test.Cardano.Ledger.Allegra.CDDL (allegraCDDL)
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Binary.Cuddle
import Test.Cardano.Ledger.Common

spec :: Spec
spec =
  describe "CDDL" $ do
    let v = eraProtVerLow @AllegraEra
    describe "Ruby-based" $ beforeAllCddlFile 3 readAllegraCddlFiles $ do
      cddlRoundTripCborSpec @(Value AllegraEra) v "coin"
      cddlRoundTripAnnCborSpec @(TxBody AllegraEra) v "transaction_body"
      cddlRoundTripAnnCborSpec @(Script AllegraEra) v "native_script"
      cddlRoundTripAnnCborSpec @(TxAuxData AllegraEra) v "auxiliary_data"

    describe "Huddle" $ specWithHuddle allegraCDDL 100 $ do
      huddleRoundTripCborSpec @(Value AllegraEra) v "coin"
      huddleRoundTripAnnCborSpec @(TxBody AllegraEra) v "transaction_body"
      huddleRoundTripAnnCborSpec @(TxAuxData AllegraEra) v "auxiliary_data"
      huddleRoundTripAnnCborSpec @(Script AllegraEra) v "native_script"
