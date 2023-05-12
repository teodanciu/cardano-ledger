{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Ledger.Alonzo (Alonzo, AlonzoEra)
import Cardano.Ledger.Core
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

import Cardano.Ledger.Crypto
import Cardano.Ledger.Language (Language (..))
import Test.Cardano.Ledger.Alonzo.Translation.Golden (generateGoldenFile)
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstanceGen (ArbitraryValidTx (..))
import Test.QuickCheck (
  Gen,
  arbitrary,
  suchThat,
 )

-- | Generates golden translation file for Alonzo era
main :: IO ()
main = generateGoldenFile @Alonzo [PlutusV1] "eras/alonzo/test-suite/golden/translations.cbor"

instance forall era. Crypto era => ArbitraryValidTx (AlonzoEra era) where
  validTx l = suchThat (arbitrary :: Gen (Tx (AlonzoEra era))) validTxInToTxOutRatio
