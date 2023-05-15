{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Translation.TranslationInstanceGen (
  ArbitraryValidTx (..),
  translationInstances,
  epochInfo,
  systemStart,
) where

import Cardano.Ledger.Language (Language (..))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)

import Test.QuickCheck (
  Arbitrary,
  Gen,
  arbitrary,
  generate,
  oneof,
  vectorOf,
 )

import Cardano.Ledger.Core as Core
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Cardano.Ledger.UTxO (UTxO (..))

import Cardano.Ledger.Alonzo.TxInfo

import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstance (
  TranslationInstance (TranslationInstance),
 )

class EraTx era => ArbitraryValidTx era where
  validTx :: Language -> Gen (Core.Tx era)
  validUTxO :: Language -> Core.Tx era -> Gen (UTxO era)

translationInstances ::
  forall era.
  ( ExtendedUTxO era
  , ArbitraryValidTx era
  , Arbitrary (PParams era)
  ) =>
  Int ->
  [Language] ->
  IO [TranslationInstance era]
translationInstances size ls =
  generate $ vectorOf size (genTranslationInstance ls)

genTranslationInstance ::
  forall era.
  ( ExtendedUTxO era
  , ArbitraryValidTx era
  , Arbitrary (PParams era)
  ) =>
  [Language] ->
  Gen (TranslationInstance era)
genTranslationInstance ls = do
  pp <- arbitrary :: Gen (PParams era)
  language <- oneof (pure <$> ls)
  tx <- validTx @era language
  fullUtxo <- validUTxO language tx
  let vtxInfoE = txInfo pp language epochInfo systemStart fullUtxo tx
  let vtxInfo = either (error . show) id vtxInfoE
  pure $ TranslationInstance pp language fullUtxo tx vtxInfo

epochInfo :: EpochInfo (Either a)
epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0
