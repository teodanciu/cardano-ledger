{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen (
  TranslatableGen (..),
  TxInfoLanguage (..),
  translationInstances,
  epochInfo,
  toVersionedTxInfo,
  systemStart,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Plutus.Context (
  ContextError,
  EraPlutusTxInfo,
  LedgerTxInfo (..),
  PlutusTxInfo,
  toPlutusTxInfo,
 )
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript (eraMaxLanguage))
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Core as Core
import Cardano.Ledger.Plutus.Language (Language (..), SLanguage (..))
import Cardano.Ledger.State (UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Stack
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstance (
  TranslationInstance (..),
  VersionedTxInfo (..),
 )
import Test.QuickCheck (Gen, arbitrary, elements, vectorOf)
import Test.QuickCheck.Gen (Gen (MkGen))
import Test.QuickCheck.Random (mkQCGen)

data TxInfoLanguage era where
  TxInfoLanguage :: EraPlutusTxInfo l era => SLanguage l -> TxInfoLanguage era

class EraTx era => TranslatableGen era where
  tgRedeemers :: Gen (Redeemers era)
  tgTx :: Language -> Gen (Core.Tx era)
  tgUtxo :: Language -> Core.Tx era -> Gen (UTxO era)
  mkTxInfoLanguage :: HasCallStack => Language -> TxInfoLanguage era

instance TranslatableGen AlonzoEra where
  tgRedeemers = arbitrary
  tgTx _ = arbitrary :: Gen (Tx AlonzoEra)
  tgUtxo _ tx = do
    let ins = tx ^. bodyTxL ^. inputsTxBodyL
    outs <- vectorOf (length ins) (arbitrary :: Gen (TxOut AlonzoEra))
    pure $ UTxO (Map.fromList $ Set.toList ins `zip` outs)
  mkTxInfoLanguage PlutusV1 = TxInfoLanguage SPlutusV1
  mkTxInfoLanguage lang =
    error $ "Language " ++ show lang ++ " is not supported in " ++ eraName @AlonzoEra

translationInstances ::
  forall era.
  ( AlonzoEraScript era
  , TranslatableGen era
  , Show (ContextError era)
  ) =>
  Int ->
  Int ->
  [TranslationInstance era]
translationInstances size seed =
  generateWithSeed seed $ vectorOf size genTranslationInstance

generateWithSeed :: Int -> Gen a -> a
generateWithSeed seed (MkGen g) = g (mkQCGen seed) 30

toVersionedTxInfo :: SLanguage l -> PlutusTxInfo l -> VersionedTxInfo
toVersionedTxInfo slang txInfo =
  case slang of
    SPlutusV1 -> TxInfoPV1 txInfo
    SPlutusV2 -> TxInfoPV2 txInfo
    SPlutusV3 -> TxInfoPV3 txInfo

genTranslationInstance ::
  forall era.
  ( AlonzoEraScript era
  , TranslatableGen era
  , Show (ContextError era)
  ) =>
  Gen (TranslationInstance era)
genTranslationInstance = do
  protVer <- arbitrary
  lang <- elements [minBound .. eraMaxLanguage @era]
  tx <- tgTx @era lang
  utxo <- tgUtxo lang tx
  let lti =
        LedgerTxInfo
          { ltiProtVer = protVer
          , ltiEpochInfo = epochInfo
          , ltiSystemStart = systemStart
          , ltiUTxO = utxo
          , ltiTx = tx
          }
  case mkTxInfoLanguage @era lang of
    TxInfoLanguage slang -> do
      case toPlutusTxInfo slang lti of
        Left err -> error $ show err
        Right txInfo -> pure $ TranslationInstance protVer lang utxo tx $ toVersionedTxInfo slang txInfo

epochInfo :: EpochInfo (Either a)
epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 1684445839000 -- 18/05/2023
