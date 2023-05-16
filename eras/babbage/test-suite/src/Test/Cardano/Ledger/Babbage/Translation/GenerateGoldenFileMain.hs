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
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstanceGen (ArbitraryValidTx (..))
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
import Test.Cardano.Ledger.Babbage.Translation.TranslatableTxGen ()

-- | Generates golden translation file for Babbage era
main :: IO ()
main = generateGoldenFile @Babbage [PlutusV2] "eras/babbage/test-suite/golden/translations.cbor"

-- instance Crypto era => ArbitraryValidTx (BabbageEra era) where
--   validTx =
--     \case
--       PlutusV1 -> v1Tx
--       PlutusV2 -> v2Tx

--   validUTxO language = utxoWithTx


-- utxoWithTx :: forall era.Crypto era => Tx (BabbageEra era) -> Gen (UTxO (BabbageEra era))
-- utxoWithTx tx = do
--   let ins = tx ^. bodyTxL ^. inputsTxBodyL
--   -- outs <- vectorOf (length ins) (arbitrary :: Gen (TxOut Babbage))
--   -- ad <- genNonByronAddr @era
--   outs <- vectorOf (length ins) (genNonByronTxOut @era)
--   pure $ UTxO (Map.fromList $ Set.toList ins `zip` [])


-- v2Tx :: forall era.Crypto era => Gen (Tx (BabbageEra era))
-- v2Tx = arbitrary :: Gen (Tx (BabbageEra era))
--   -- do
--   --   tx <- arbitrary :: Gen (Tx Babbage)
--   --   txouts <- listOf1 genNonByronTxOut
--   --   let txBody = (trace ("Setting addresses: " <> show (fmap (\txout -> txout ^.addrTxOutL) txouts))) tx ^. bodyTxL
--   --   pure $ tx & bodyTxL .~ (txBody & outputsTxBodyL .~ fromList txouts)

-- v1Tx :: forall era.Crypto era => Gen (Tx (BabbageEra era))
-- v1Tx = arbitrary

-- -- txBodyWithShelleyAddresses :: TxBody Babbage -> TxBody Babbage
-- -- txBodyWithShelleyAddresses txBody =
-- --   let txOuts = fmap (\txout -> txout & addrTxOutL .~ shelleyAddr) $ txBody ^.outputsTxBodyL
-- --   in txBody & outputsTxBodyL .~ txOuts


-- -- -- -- withShelleyAddr :: forall era . Crypto era =>  TxOut (BabbageEra era) -> TxOut (BabbageEra era)
-- -- -- -- withShelleyAddr txout = txout & addrTxOutL .~ shelleyAddr
-- -- shelleyAddr :: Addr StandardCrypto
-- -- shelleyAddr = Addr Testnet alicePHK StakeRefNull

-- -- -- genNonByronAddr :: forall era . Crypto era => Gen (Addr (EraCrypto (BabbageEra era)))
-- -- -- genNonByronAddr = arbitrary :: Gen ( Addr (EraCrypto (BabbageEra era)))
-- genNonByronAddr :: forall era . Crypto era => Gen (Addr era)
-- genNonByronAddr = arbitrary :: Gen ( Addr era)
--   -- x <- arbitrary :: Gen (PaymentCredential Babbage)
--   -- pure $ Addr Testnet x StakeRefNull

-- genNonByronTxOut :: forall era. Crypto era => Gen (BabbageTxOut (BabbageEra era))
-- genNonByronTxOut = do
--     addr <- genNonByronAddr @era
--     pure (BabbageTxOut addr)
--       -- <*> scale (`div` 15) arbitrary
--       <*> arbitrary
--       <*> arbitrary
--       <*> arbitrary
