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

module Test.Cardano.Ledger.Babbage.Translation.TranslatableGen2 (
  genNonByronAddr,
  genTxOut,
  genTxWits,
  genTx,
  utxoWithTx
) where

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
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag(..), AlonzoScript (..))
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
  tgTx l = genTx @(BabbageEra era) l (genTxBody l)
  tgUtxo = utxoWithTx @(BabbageEra era)

-- utxoWithTx :: forall era.Crypto era => Language -> Tx (BabbageEra era) -> Gen (UTxO (BabbageEra era))
-- utxoWithTx l tx = do
--   let ins = tx ^. bodyTxL ^. inputsTxBodyL
--   let refIns = tx ^. bodyTxL ^. referenceInputsTxBodyL
--   let allIns = ins `Set.union` refIns
--   outs <- vectorOf (length allIns) (genTxOut @era l)
--   pure $ UTxO (Map.fromList $ Set.toList allIns `zip` outs)

utxoWithTx ::
  forall era.
  ( EraTx era
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  , BabbageEraTxBody era
  , TxOut era ~ BabbageTxOut era
  ) => Language -> Tx era -> Gen (UTxO era)
utxoWithTx l tx = do
  let ins = tx ^. bodyTxL ^. inputsTxBodyL
  let refIns = tx ^. bodyTxL ^. referenceInputsTxBodyL
  let allIns = ins `Set.union` refIns
  outs <- vectorOf (length allIns) (genTxOut @era l)
  pure $ UTxO (Map.fromList $ Set.toList allIns `zip` outs)

genTx :: forall era.
  (
    Arbitrary (TxAuxData era)
  , Arbitrary (Script era)
  , AlonzoEraTxWits era
  , AlonzoScript era ~ Script era
  , AlonzoTxWits era ~ TxWits era
  ) => Language -> Gen (TxBody era) -> Gen (AlonzoTx era)
genTx l txbGen =  AlonzoTx
      <$> txbGen
      <*> genTxWits @era l
      <*> arbitrary
      <*> arbitrary

-- genTx :: forall era. Crypto era => Language -> Gen (AlonzoTx (BabbageEra era))
-- genTx l =  AlonzoTx
--       <$> genTxBody @era l
--       <*> genTxWits @(BabbageEra era) l
--       <*> arbitrary
--       <*> arbitrary

genTxWits ::
  forall era.
  ( Arbitrary (Script era)
  , AlonzoEraTxWits era
  , AlonzoScript era ~ Script era
  , AlonzoTxWits era ~ TxWits era
  ) => Language -> Gen (AlonzoTxWits era)
genTxWits = \case
  PlutusV1 -> arbitrary
  _ -> do
      arbWits <- arbitrary :: Gen (AlonzoTxWits era)
      pure $ arbWits & rdmrsTxWitsL @era
          .~ Redeemers (Map.singleton (RdmrPtr Spend 0) (Data (PV1.I 42), ExUnits 5000 5000))

genTxBody :: forall era.Crypto era => Language -> Gen (BabbageTxBody (BabbageEra era) )
genTxBody l = do
  let genTxOuts = fromList <$> listOf1 (mkSized (eraProtVerLow @Babbage) <$> genTxOut @(BabbageEra era) l)
  let genTxIns = Set.fromList <$> listOf1 (arbitrary :: Gen (TxIn era))
  BabbageTxBody
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
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary


genTxOut ::
  forall era.
  ( EraTxOut era
  , Arbitrary (Value era)
  , Arbitrary (Script era)
  ) => Language -> Gen (BabbageTxOut era)
genTxOut l = do
  addr <- genNonByronAddr @(EraCrypto era)
  value <- scale (`div` 15) arbitrary
  script <- case l of
        PlutusV1 -> pure SNothing
        _ -> arbitrary
  datum <- case l of
        PlutusV1 -> oneof [ pure NoDatum, DatumHash <$> (arbitrary :: Gen (DataHash (EraCrypto era))) ]
        _  -> arbitrary
  pure $ BabbageTxOut addr value datum script


genNonByronAddr :: forall era . Crypto era => Gen (Addr era)
genNonByronAddr = Addr <$> arbitrary <*> arbitrary <*> frequency
    [ (85, StakeRefBase <$> arbitrary)
    , (15, pure StakeRefNull)
    ]
