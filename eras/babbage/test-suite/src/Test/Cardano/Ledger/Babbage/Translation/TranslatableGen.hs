{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Translation.TranslatableGen where

import Cardano.Ledger.Babbage (Babbage, BabbageEra)
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

instance forall era. Crypto era => TranslatableGen (BabbageEra era) where
  tgTx  = genTx
  tgUtxo = utxoWithTx

utxoWithTx :: forall era.Crypto era => Language -> Tx (BabbageEra era) -> Gen (UTxO (BabbageEra era))
utxoWithTx l tx = do
  let ins = tx ^. bodyTxL ^. inputsTxBodyL
  let refIns = tx ^. bodyTxL ^. referenceInputsTxBodyL
  let allIns = ins `Set.union` refIns
  outs <- vectorOf (length allIns) (genTxOut @era l)
  pure $ UTxO (Map.fromList $ Set.toList allIns `zip` outs)

genTx :: forall era. Crypto era => Language -> Gen (AlonzoTx (BabbageEra era))
genTx l =  AlonzoTx
      <$> genTxBody l
      <*> genTxWits l
      <*> arbitrary
      <*> arbitrary

genTxWits :: forall era. Crypto era => Language -> Gen (AlonzoTxWits (BabbageEra era))
genTxWits = \case
  PlutusV1 -> arbitrary
  PlutusV2 -> do
      arbWits <- arbitrary :: Gen (AlonzoTxWits (BabbageEra era))
      pure $ arbWits & rdmrsTxWitsL @(BabbageEra era)
          .~ Redeemers (Map.singleton (RdmrPtr Spend 0) (Data (PV1.I 42), ExUnits 5000 5000))

genTxBody :: forall era.Crypto era => Language -> Gen (BabbageTxBody (BabbageEra era) )
genTxBody l = do
  let genTxOuts = fromList <$> listOf1 (mkSized (eraProtVerLow @Babbage) <$> genTxOut @era l)
  let genTxIns = Set.fromList <$> listOf1 (arbitrary :: Gen (TxIn era))
  BabbageTxBody
      <$> genTxIns
      <*> arbitrary
      <*> ( case l of ----refinputs
              PlutusV1 -> pure Set.empty
              PlutusV2 -> arbitrary
          )
      <*> genTxOuts
      <*> arbitrary
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


genTxOut :: forall era. Crypto era => Language -> Gen (BabbageTxOut (BabbageEra era))
genTxOut l = do
  addr <- genNonByronAddr @era
  value <- scale (`div` 15) arbitrary
  script <- case l of
        PlutusV1 -> pure SNothing
        PlutusV2 -> arbitrary
  datum <- case l of
        PlutusV1 -> oneof [ pure NoDatum, DatumHash <$> (arbitrary :: Gen (DataHash era)) ]
        PlutusV2 -> arbitrary
  pure $ BabbageTxOut addr value datum script

genNonByronAddr :: forall era . Crypto era => Gen (Addr era)
genNonByronAddr = Addr <$> arbitrary <*> arbitrary <*> frequency
    [ (85, StakeRefBase <$> arbitrary)
    , (15, pure StakeRefNull)
    ]
