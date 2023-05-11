{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.GenGoldenTranslationFile (
  genGoldenFile,
) where

import Cardano.Ledger.Core

import Cardano.Ledger.Binary.Encoding (serialize)
import qualified Data.ByteString.Lazy as BSL
import Paths_cardano_ledger_alonzo_test ()

import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO)
import Cardano.Ledger.Language (Language)
import Test.Cardano.Ledger.Alonzo.TranslationInstance (ArbitraryValidTx (..), translationInstances)
import Test.QuickCheck (Arbitrary)

-- | Generates arguments for `ExtendedUTxO.txInfo`, applies them to it
-- and serializes both arguments and result to golden/translations.cbor file
genGoldenFile ::
  forall era.
  ( ExtendedUTxO era
  , ArbitraryValidTx era
  , Arbitrary (PParams era)
  ) =>
  [Language] ->
  FilePath ->
  IO ()
genGoldenFile ls file =
  do
    putStrLn "Generating golden files for TxInfo"
    instances <- translationInstances @era 1 ls
    let cbor = serialize (eraProtVerHigh @era) instances
    BSL.writeFile file cbor
