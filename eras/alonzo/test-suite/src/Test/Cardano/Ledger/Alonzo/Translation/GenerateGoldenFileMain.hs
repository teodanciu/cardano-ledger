{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstanceGen (ArbitraryValidTx (..))
import Test.QuickCheck (
  Gen,
  arbitrary,
  suchThat,
  listOf1,
  vectorOf,
  scale
 )

import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..), AlonzoTxBody (..))

-- | Generates golden translation file for Alonzo era
main :: IO ()
main = generateGoldenFile @Alonzo [PlutusV1] "eras/alonzo/test-suite/golden/translations.cbor"

instance Crypto era => ArbitraryValidTx (AlonzoEra era) where
  validTx _ = arbitrary :: Gen (Tx (AlonzoEra era))
  -- do
  --   tx <- arbitrary :: Gen (Tx (AlonzoEra era))
  --   nonEmptyIns <- listOf1 (arbitrary :: Gen (TxIn era))
  --   let body = tx ^. bodyTxL
  --   let nonEmptyInsBody = body & inputsTxBodyL .~ Set.fromList nonEmptyIns
  --   -- pure $ tx & bodyTxL .~ body
  --   pure tx

  validUTxO _ tx = do
    let ins = tx ^. bodyTxL ^. inputsTxBodyL
    outs <- vectorOf (length ins) (arbitrary :: Gen (TxOut (AlonzoEra era)))
    pure $ UTxO (Map.fromList $ Set.toList ins `zip` outs)
