{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Core
import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Crypto
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()

import Cardano.Ledger.Binary.Encoding (serialize)
import qualified Data.ByteString.Lazy as BSL

import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..))
-- import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
-- import Cardano.Ledger.Babbage.Tx (AlonzoEraTx)
import Cardano.Ledger.Babbage.TxBody (Datum (..), BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (StakeReference (..), PaymentCredential (..))
import Cardano.Ledger.Language (Language (..))
import Data.Maybe.Strict
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace (trace)
import Test.Cardano.Ledger.Alonzo.Translation.Golden (generateGoldenFile)
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstanceGen (TranslatableGen (..))
import Test.Cardano.Ledger.Shelley.Examples.Cast
import Test.QuickCheck (
  Gen,
  arbitrary,
  suchThat,
  vectorOf,
  Arbitrary,
  listOf1
 )
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Alonzo.Tx (rdptrInv)
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag(..))
import qualified PlutusLedgerApi.V1 as PV1
import Cardano.Ledger.Alonzo.TxWits
import Test.Cardano.Ledger.Shelley.Examples.Cast (alicePHK)
import Cardano.Ledger.Alonzo (Alonzo)
import Data.Sequence.Strict (fromList)
import Debug.Trace (trace)
import Test.Cardano.Ledger.Core.Arbitrary ()
-- import Test.Cardano.Ledger.Babbage.Translation.TranslatableGen ()
import qualified Test.Cardano.Ledger.Babbage.Translation.TranslatableGen2
  as BabbageTranslatableGen(genTx, genTxOut, utxoWithTx)


import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()

import Cardano.Ledger.Binary.Encoding (serialize)
import qualified Data.ByteString.Lazy as BSL

import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Babbage.TxBody (Datum (..), BabbageTxOut (..), BabbageTxBody (..))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (StakeReference (..), PaymentCredential (..))
import Cardano.Ledger.Language (Language (..))
import Data.Maybe.Strict
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace (trace)
import Test.Cardano.Ledger.Alonzo.Translation.Golden (generateGoldenFile)
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstanceGen (TranslatableGen (..))
import Test.Cardano.Ledger.Shelley.Examples.Cast
import Test.QuickCheck (
  Gen,
  arbitrary,
  frequency,
  suchThat,
  vectorOf,
  Arbitrary,
  listOf1,
  scale,
  oneof
 )
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Alonzo.Tx (rdptrInv)
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag(..))
import qualified PlutusLedgerApi.V1 as PV1
import Cardano.Ledger.Alonzo.TxWits
import Test.Cardano.Ledger.Shelley.Examples.Cast (alicePHK)
import Cardano.Ledger.Alonzo (Alonzo)
import Data.Sequence.Strict (fromList)
import Debug.Trace (trace)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Cardano.Ledger.Binary (
  Sized (..),
  mkSized,
 )
import Cardano.Ledger.Binary.Version (natVersion)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag(..), AlonzoScript (..))
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))


-- | Generates golden translation file for Conway era
main :: IO ()
main = generateGoldenFile @Conway [PlutusV1, PlutusV2, PlutusV3] "eras/conway/test-suite/golden/translations.cbor"

instance forall era. Crypto era => TranslatableGen (ConwayEra era) where
  tgTx l = BabbageTranslatableGen.genTx @(ConwayEra era) l (genTxBody l)
  tgUtxo = BabbageTranslatableGen.utxoWithTx @(ConwayEra era)

genTxBody :: forall era.Crypto era => Language -> Gen (ConwayTxBody (ConwayEra era) )
genTxBody l = do
    let genTxOuts = fromList <$>
                      listOf1 (mkSized (eraProtVerLow @Conway) <$>
                      BabbageTranslatableGen.genTxOut @(ConwayEra era) l)
    let genTxIns = Set.fromList <$> listOf1 (arbitrary :: Gen (TxIn era))
    ConwayTxBody
      <$> genTxIns
      <*> arbitrary
      <*> ( case l of ----refinputs
              PlutusV1 -> pure Set.empty
              _ -> arbitrary
          )
      <*> genTxOuts
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
