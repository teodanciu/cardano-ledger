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

import Cardano.Ledger.Babbage (Babbage, BabbageEra)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()

import Cardano.Ledger.Binary.Encoding (serialize)
import qualified Data.ByteString.Lazy as BSL

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Babbage.Tx (AlonzoEraTx)
import Cardano.Ledger.Babbage.TxBody (Datum (..))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (StakeReference (..))
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
 )
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Ledger.Alonzo.Tx (rdptrInv)
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag(..))
import qualified PlutusLedgerApi.V1 as PV1
import Cardano.Ledger.Alonzo.TxWits

-- | Generates golden translation file for Babbage era
main :: IO ()
main = generateGoldenFile @Babbage [PlutusV2] "eras/babbage/test-suite/golden/translations.cbor"

instance Crypto era => TranslatableGen (BabbageEra era) where
  validTx = \case
    PlutusV1 -> suchThat genValidPlutusV1Tx $ \tx ->
        validPlutusV1TxInToTxOutRatio tx
          && shelleyAddresses (tx ^. (bodyTxL . outputsTxBodyL))
    PlutusV2 -> suchThat genValidPlutusV2Tx $ \tx ->
        plutusV2validPlutusV1TxInToTxOutRatio tx
          && shelleyAddresses (tx ^. (bodyTxL . outputsTxBodyL))
  validUTxO = \case
    PlutusV1 ->  utxoWithTx
    PlutusV2 -> plutusV2UtxoWithTxWithRefInputs


genValidPlutusV1Tx :: forall era. Crypto era => Gen (Tx (BabbageEra era))
genValidPlutusV1Tx = do
        tx <- arbitrary @(Tx (BabbageEra era))
        pure $ tx & (bodyTxL .~ validPlutusV1TxBody (tx ^. bodyTxL))

genValidPlutusV2Tx :: forall era. Crypto era => Gen (Tx (BabbageEra era))
genValidPlutusV2Tx = do
        tx <- arbitrary @(Tx (BabbageEra era))
        pure $ tx & witsTxL .~ setWits (tx ^. witsTxL)

setWits :: forall era. Crypto era => AlonzoTxWits (BabbageEra era) ->  AlonzoTxWits (BabbageEra era)
setWits = rdmrsTxWitsL @(BabbageEra era)
          .~ Redeemers (Map.singleton (RdmrPtr Spend 0) (Data (PV1.I 42), ExUnits 5000 5000))

validPlutusV1TxInToTxOutRatio tx =
  let txBody = tx ^. bodyTxL
      txIns = txBody ^. inputsTxBodyL
      txOuts = txBody ^. outputsTxBodyL
   --so that when we zip them to build the utxo, we don't lose any inputs that are then checked against the utxo
   in Set.size txIns > 0 && length txOuts >= Set.size txIns

plutusV2validPlutusV1TxInToTxOutRatio tx =
  let txBody = tx ^. bodyTxL
      txIns = txBody ^. inputsTxBodyL
      refTxIns = txBody ^. referenceInputsTxBodyL
      allTxIns = txIns <> refTxIns
      txOuts = txBody ^. outputsTxBodyL
   --so that when we zip them to build the utxo, we don't lose any inputs that are then checked against the utxo
   in Set.size txIns > 0 && length txOuts >= Set.size allTxIns

utxoWithTx tx =
  let txBody = tx ^. bodyTxL
      txIns = Set.toList $ txBody ^. inputsTxBodyL
      txOuts = foldr (:) [] $ txBody ^. outputsTxBodyL
   in UTxO (Map.fromList $ txIns `zip` txOuts)

plutusV2UtxoWithTxWithRefInputs tx =
  let txBody = tx ^. bodyTxL
      txIns = txBody ^. inputsTxBodyL
      refTxIns = txBody ^. referenceInputsTxBodyL
      allTxIns = Set.toList (txIns <> refTxIns)
      txOuts = foldr (:) [] $ txBody ^. outputsTxBodyL
   in UTxO (Map.fromList $ allTxIns `zip` txOuts)

validPlutusV1TxBody :: forall era. Crypto era => TxBody (BabbageEra era) -> TxBody (BabbageEra era)
validPlutusV1TxBody txbody =
  txbody
    & referenceInputsTxBodyL @(BabbageEra era) .~ Set.empty
    & outputsTxBodyL .~ validTxOuts (txbody ^. outputsTxBodyL)

validTxOuts :: forall era. Crypto era => StrictSeq (TxOut (BabbageEra era)) -> StrictSeq (TxOut (BabbageEra era))
validTxOuts =
  fmap
    ( \txout ->
        txout
          & referenceScriptTxOutL .~ SNothing
          & datumTxOutL .~ NoDatum
    )
shelleyAddresses :: forall era.Crypto era => StrictSeq (TxOut (BabbageEra era)) -> Bool
shelleyAddresses =
    all
      ( \out ->
          (out ^. bootAddrTxOutF) == Nothing
            && isShelleyAddr out
      )
isShelleyAddr txout = (txout ^. bootAddrTxOutF) == Nothing && addrNotBootstrap (txout ^. addrTxOutL)
addrNotBootstrap addr = case addr of
  AddrBootstrap _ -> False
  Addr nw _ _ -> True


  -- validTx l = case l of
  --   PlutusV1 ->
  --     suchThat plutusV1Body $ \tx ->
  --       validTxInToTxOutRatio tx && shelleyAddresses (tx ^. (bodyTxL . outputsTxBodyL))
  --   PlutusV2 -> suchThat (arbitrary @(Tx (BabbageEra era))) $ \tx ->
  --     validTxInToTxOutRatio tx && shelleyAddresses (tx ^. (bodyTxL . outputsTxBodyL))
  --   where
  --     plutusV1Body = (\tx -> tx & (bodyTxL .~ validTxBody (tx ^. bodyTxL))) <$> arbitrary @(Tx (BabbageEra era))
  --     validTxBody txbody =
  --       txbody
  --         & referenceInputsTxBodyL .~ Set.empty
  --         & outputsTxBodyL .~ validTxOuts (txbody ^. outputsTxBodyL)
  --     validTxOuts =
  --       fmap
  --         ( \txout ->
  --             txout
  --               & referenceScriptTxOutL .~ SNothing
  --               & datumTxOutL .~ NoDatum
  --         )
  --     shelleyAddresses =
  --       all
  --         ( \out ->
  --             (out ^. bootAddrTxOutF) == Nothing
  --               && isShelleyAddr out
  --         )
  --     isShelleyAddr txout = (txout ^. bootAddrTxOutF) == Nothing && addrNotBootstrap (txout ^. addrTxOutL)
  --     addrNotBootstrap addr = case addr of
  --       AddrBootstrap _ -> False
  --       Addr nw _ _ -> True
  -- validUTxO l tx = \case
