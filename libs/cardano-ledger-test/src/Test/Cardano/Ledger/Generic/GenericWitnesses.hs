{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Generic.GenericWitnesses
  ( witsVKeyNeeded',
    neededDataHashes,
    neededRedeemers,
    txOutLookupDatum,
    rdptrInv',
  )
where

import Cardano.Ledger.Alonzo.PlutusScriptApi (scriptsNeededFromBody)
import qualified Cardano.Ledger.Alonzo.Rules.Utxow as Alonzo
import Cardano.Ledger.Alonzo.Tx ( rdptr, ScriptPurpose )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (inputDataHashes))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr)
import Cardano.Ledger.Babbage.Tx (ValidatedTx (..))
import Cardano.Ledger.Babbage.TxBody (Datum)
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import Cardano.Ledger.Core (TxBody, TxOut)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Hashes (DataHash, ScriptHash)
import Cardano.Ledger.Keys (GenDelegs, KeyRole (Witness))
import Cardano.Ledger.Shelley.API (KeyHash)
import Cardano.Ledger.Shelley.LedgerState
  ( WitHashes (..),
  )
import qualified Cardano.Ledger.Shelley.Rules.Utxow as Shelley
import Cardano.Ledger.Shelley.UTxO (UTxO)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Maybe.Strict
    ( strictMaybeToMaybe, StrictMaybe(SNothing), StrictMaybe(SJust) )
import Data.Set (Set)
import Test.Cardano.Ledger.Generic.Fields (TxField (Body), initialTx)
import Test.Cardano.Ledger.Generic.Proof (Proof (..))
import Test.Cardano.Ledger.Generic.Updaters (updateTx)
import qualified Cardano.Ledger.Babbage.Tx as BabbageTx
import qualified Cardano.Ledger.Alonzo.Tx as AlonzoTx

witsVKeyNeeded' ::
  forall era.
  Proof era ->
  UTxO era ->
  Core.TxBody era ->
  GenDelegs (Crypto era) ->
  Set (KeyHash 'Witness (Crypto era))
witsVKeyNeeded' proof utxo' txbody gd =
  unWitHashes $
    let tx = updateTx proof (initialTx proof) $ Body txbody
     in case proof of
          (Babbage _) -> Alonzo.witsVKeyNeeded utxo' tx gd
          (Alonzo _) -> Alonzo.witsVKeyNeeded utxo' tx gd
          (Shelley _) -> Shelley.witsVKeyNeeded utxo' tx gd
          (Mary _) -> Shelley.witsVKeyNeeded utxo' tx gd
          (Allegra _) -> Shelley.witsVKeyNeeded utxo' tx gd

neededDataHashes ::
  Proof era ->
  Map.Map (ScriptHash (Crypto era)) (Core.Script era) ->
  TxBody era ->
  UTxO era ->
  Set (DataHash (Crypto era))
neededDataHashes proof m txbody utxo =
  let tx = updateTx proof (initialTx proof) $ Body txbody
   in case proof of
        (Babbage _) -> fst $ inputDataHashes m tx utxo
        (Alonzo _) -> fst $ inputDataHashes m tx utxo
        _ -> mempty

neededRedeemers ::
  Proof era ->
  UTxO era ->
  TxBody era ->
  [RdmrPtr]
neededRedeemers proof utxo txbody = case proof of
  Babbage _ -> catMaybes (tryGetRdmrPtr <$> scriptsNeededFromBody utxo txbody)
    where
      tryGetRdmrPtr = strictMaybeToMaybe . rdptr txbody . fst
  Alonzo _ -> catMaybes (tryGetRdmrPtr <$> scriptsNeededFromBody utxo txbody)
    where
      tryGetRdmrPtr = strictMaybeToMaybe . rdptr txbody . fst
  _ -> []

txOutLookupDatum :: Proof era -> TxOut era -> Datum era
txOutLookupDatum (Babbage _) (Babbage.TxOut _ _ d _) = d
txOutLookupDatum (Alonzo _) (Alonzo.TxOut _ _ d) = case d of
  SJust d' -> Babbage.DatumHash d'
  SNothing -> Babbage.NoDatum
txOutLookupDatum (Allegra _) _ = Babbage.NoDatum
txOutLookupDatum (Mary _) _ = Babbage.NoDatum
txOutLookupDatum (Shelley _) _ = Babbage.NoDatum

rdptrInv' ::
  Proof era ->
  Core.TxBody era ->
  RdmrPtr ->
  StrictMaybe (ScriptPurpose (Crypto era))
rdptrInv' (Babbage _) txbody ptr = BabbageTx.rdptrInv txbody ptr
rdptrInv' (Alonzo _) txbody ptr = AlonzoTx.rdptrInv txbody ptr
rdptrInv' _ _ _ = SNothing

