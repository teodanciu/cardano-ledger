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
import Test.Cardano.Ledger.Babbage.Translation.TranslatableGen ()

-- | Generates golden translation file for Babbage era
main :: IO ()
main = generateGoldenFile @Babbage [PlutusV1, PlutusV2] "eras/babbage/test-suite/golden/translations.cbor"
