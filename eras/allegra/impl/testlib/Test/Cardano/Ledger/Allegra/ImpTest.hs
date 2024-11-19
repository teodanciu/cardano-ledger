{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.ImpTest (
  impAllegraSatisfyNativeScript,
  module Test.Cardano.Ledger.Shelley.ImpTest,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN)
import Cardano.Crypto.Hash.Blake2b (Blake2b_224)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript,
  evalTimelock,
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Control.Monad.State.Strict (get)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Allegra.TreeDiff ()
import Test.Cardano.Ledger.Core.KeyPair (KeyPair)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , ADDRHASH c ~ Blake2b_224
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (AllegraEra c)
  where
  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  fixupTx = shelleyFixupTx

impAllegraSatisfyNativeScript ::
  ( AllegraEraScript era
  , AllegraEraTxBody era
  ) =>
  Set.Set (KeyHash 'Witness (EraCrypto era)) ->
  TxBody era ->
  NativeScript era ->
  ImpTestM era (Maybe (Map.Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era))))
impAllegraSatisfyNativeScript providedVKeyHashes txBody script = do
  impState <- get
  let
    keyPairs = impState ^. impKeyPairsG
    vi = txBody ^. vldtTxBodyL
    satisfyMOf m Empty
      | m <= 0 = Just mempty
      | otherwise = Nothing
    satisfyMOf m (x :<| xs) =
      case satisfyScript x of
        Nothing -> satisfyMOf m xs
        Just kps -> do
          kps' <- satisfyMOf (m - 1) xs
          Just $ kps <> kps'
    satisfyScript = \case
      RequireSignature keyHash
        | keyHash `Set.member` providedVKeyHashes -> Just mempty
        | otherwise -> do
            keyPair <- Map.lookup keyHash keyPairs
            Just $ Map.singleton keyHash keyPair
      RequireAllOf ss -> satisfyMOf (length ss) ss
      RequireAnyOf ss -> satisfyMOf 1 ss
      RequireMOf m ss -> satisfyMOf m ss
      lock@(RequireTimeStart _)
        | evalTimelock mempty vi lock -> Just mempty
        | otherwise -> Nothing
      lock@(RequireTimeExpire _)
        | evalTimelock mempty vi lock -> Just mempty
        | otherwise -> Nothing
  pure $ satisfyScript script
