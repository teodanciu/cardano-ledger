{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.TranslationInstance (
  ArbitraryValidTx (..),
  TranslationInstance (..),
  translationInstances,
  epochInfo,
  systemStart,
  deserializeTranslationInstances,
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

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  DecoderError,
  EncCBOR (..),
  decodeFullAnnotator,
  decodeList,
  fromPlainDecoder,
  fromPlainEncoding,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<*!),
 )
import Cardano.Ledger.UTxO (UTxO (..))
import qualified Codec.Serialise as Cborg (Serialise (..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V3 as PV3

import Cardano.Ledger.Alonzo.TxInfo

import Data.Typeable

-- | Represents arguments passed to `alonzoTxInfo` along with the produced result.
data TranslationInstance era = TranslationInstance
  { tiPparams :: PParams era
  , tiLanguage :: Language
  , tiUtxo :: UTxO era
  , tiTx :: Core.Tx era
  , tiResult :: VersionedTxInfo
  }
  deriving (Generic)

deriving instance (Era era, Eq (PParams era), Eq (UTxO era), Eq (Core.Tx era)) => Eq (TranslationInstance era)
deriving instance (Era era, Show (PParams era), Show (UTxO era), Show (Core.Tx era)) => Show (TranslationInstance era)

class (EraTx era, Arbitrary (Core.Tx era)) => ArbitraryValidTx era where
  validTx :: Language -> Gen (Core.Tx era)
  validTxInToTxOutRatio :: Core.Tx era -> Bool
  validTxInToTxOutRatio =
    let txInToTxOutRatio tx =
          let txBody = tx ^. bodyTxL
              txIns = txBody ^. inputsTxBodyL
              txOuts = txBody ^. outputsTxBodyL
           in Set.size txIns > 0 && length txOuts >= Set.size txIns
     in txInToTxOutRatio

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
  let fullUtxo = utxoWithTx tx
  let vtxInfoE = txInfo pp language epochInfo systemStart fullUtxo tx
  let vtxInfo = either (error . show) id vtxInfoE
  pure $ TranslationInstance pp language fullUtxo tx vtxInfo

epochInfo :: EpochInfo (Either a)
epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0

utxoWithTx :: forall era. (EraTx era) => Core.Tx era -> UTxO era
utxoWithTx tx =
  let txBody = tx ^. bodyTxL
      txIns = Set.toList $ txBody ^. inputsTxBodyL
      txOuts = foldr (:) [] $ txBody ^. outputsTxBodyL
   in UTxO (Map.fromList $ txIns `zip` txOuts)

instance Cborg.Serialise PV1.TxInfo
instance Cborg.Serialise PV1.TxInInfo
instance Cborg.Serialise PV1.TxOut
instance Cborg.Serialise PV3.POSIXTime
instance Cborg.Serialise a => Cborg.Serialise (PV3.Extended a)
instance Cborg.Serialise a => Cborg.Serialise (PV3.LowerBound a)
instance Cborg.Serialise a => Cborg.Serialise (PV3.UpperBound a)
instance Cborg.Serialise a => Cborg.Serialise (PV3.Interval a)
instance Cborg.Serialise PV3.Address
instance Cborg.Serialise PV3.Credential
instance Cborg.Serialise PV3.CurrencySymbol
instance Cborg.Serialise PV3.DCert
instance Cborg.Serialise PV3.TxOutRef
instance Cborg.Serialise PV3.TxId
instance Cborg.Serialise PV3.Value
instance Cborg.Serialise PV3.PubKeyHash
instance (Cborg.Serialise k, Cborg.Serialise v) => Cborg.Serialise (PV3.Map k v)
instance Cborg.Serialise PV3.TokenName
instance Cborg.Serialise PV3.TxInInfo
instance Cborg.Serialise PV3.DatumHash
instance Cborg.Serialise PV3.StakingCredential
instance Cborg.Serialise PV3.ScriptHash
instance Cborg.Serialise PV3.TxOut
instance Cborg.Serialise PV3.OutputDatum
instance Cborg.Serialise PV3.ScriptPurpose
instance Cborg.Serialise PV3.TxInfo
instance Cborg.Serialise VersionedTxInfo

instance EncCBOR VersionedTxInfo where
  encCBOR = fromPlainEncoding . Cborg.encode

instance DecCBOR VersionedTxInfo where
  decCBOR = fromPlainDecoder Cborg.decode

instance
  ( Typeable era
  , EncCBOR (PParams era)
  , EncCBOR (UTxO era)
  , EncCBOR (Core.Tx era)
  ) =>
  EncCBOR (TranslationInstance era)
  where
  encCBOR (TranslationInstance pp l u tx r) =
    encode $
      Rec TranslationInstance
        !> To pp
        !> To l
        !> To u
        !> To tx
        !> To r

instance
  ( Typeable era
  , DecCBOR (PParams era)
  , DecCBOR (UTxO era)
  , DecCBOR (Annotator (Core.Tx era))
  ) =>
  DecCBOR (Annotator (TranslationInstance era))
  where
  decCBOR =
    decode $
      Ann (RecD TranslationInstance)
        <*! Ann From
        <*! Ann From
        <*! Ann From
        <*! From
        <*! Ann From

deserializeTranslationInstances ::
  forall era.
  ( Era era
  , DecCBOR (PParams era)
  , DecCBOR (UTxO era)
  , DecCBOR (Annotator (Core.Tx era))
  ) =>
  BSL.ByteString ->
  Either DecoderError [TranslationInstance era]
deserializeTranslationInstances = decodeFullAnnotator (eraProtVerHigh @era) "Translations" decList
  where
    decList = sequence <$> decodeList decCBOR
