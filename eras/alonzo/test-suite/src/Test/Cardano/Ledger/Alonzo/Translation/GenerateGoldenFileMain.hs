{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Ledger.Alonzo (Alonzo, AlonzoEra)
import Cardano.Ledger.Core
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

import Cardano.Ledger.Crypto
import Cardano.Ledger.Language (Language (..))
import Cardano.Ledger.UTxO (UTxO (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((^.))
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
  validTx _ = suchThat (arbitrary :: Gen (Tx (AlonzoEra era))) validTxInToTxOutRatio
  validUTxO _ = utxoWithTx

validTxInToTxOutRatio tx =
  let txBody = tx ^. bodyTxL
      txIns = txBody ^. inputsTxBodyL
      txOuts = txBody ^. outputsTxBodyL
   in -- so that when we zip them to build the utxo, we don't lose any inputs that are then checked against the utxo
      Set.size txIns > 0 && length txOuts >= Set.size txIns

utxoWithTx tx =
  let txBody = tx ^. bodyTxL
      txIns = Set.toList $ txBody ^. inputsTxBodyL
      txOuts = foldr (:) [] $ txBody ^. outputsTxBodyL
   in UTxO (Map.fromList $ txIns `zip` txOuts)
