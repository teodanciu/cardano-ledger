{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Ledger.Alonzo (Alonzo, AlonzoEra)
import Cardano.Ledger.Core
import Cardano.Ledger.TxIn (TxIn)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

import Cardano.Ledger.Crypto
import Cardano.Ledger.Language (Language (..))
import Cardano.Ledger.UTxO (UTxO (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Translation.Golden (generateGoldenFile)
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstanceGen (TranslatableGen (..))
import Test.QuickCheck (
  Gen,
  arbitrary,
  listOf1,
  scale,
  suchThat,
  vectorOf,
 )

import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), AlonzoTxBody (..))

-- | Generates golden translation file for Alonzo era
main :: IO ()
main = generateGoldenFile @Alonzo [PlutusV1] "eras/alonzo/test-suite/golden/translations.cbor"

instance TranslatableGen Alonzo where
  tgTx _ = arbitrary :: Gen (Tx Alonzo)
  tgUtxo _ tx = do
    let ins = tx ^. bodyTxL ^. inputsTxBodyL
    outs <- vectorOf (length ins) (arbitrary :: Gen (TxOut Alonzo))
    pure $ UTxO (Map.fromList $ Set.toList ins `zip` outs)
