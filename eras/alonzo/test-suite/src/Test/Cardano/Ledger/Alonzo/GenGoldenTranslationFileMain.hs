{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Alonzo (Alonzo, AlonzoEra)
import Cardano.Ledger.Core
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

import Cardano.Ledger.Binary.Encoding (serialize)
import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_alonzo_test ()

import Cardano.Ledger.Crypto
import Cardano.Ledger.Language (Language (..))
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Alonzo.GenGoldenTranslationFile (genGoldenFile)
import Test.Cardano.Ledger.Alonzo.TranslationInstance (ArbitraryValidTx (..), translationInstances)
import Test.QuickCheck (
  Gen,
  arbitrary,
  suchThat,
 )

-- | Generates golden translation file for Alonzo era
main :: IO ()
main = genGoldenFile @Alonzo [PlutusV1] "eras/alonzo/test-suite/golden/translations.cbor"

instance forall era. Crypto era => ArbitraryValidTx (AlonzoEra era) where
  validTx l = suchThat (arbitrary :: Gen (Tx (AlonzoEra era))) validTxInToTxOutRatio
