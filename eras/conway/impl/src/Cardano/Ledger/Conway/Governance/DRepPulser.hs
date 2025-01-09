{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Governance.DRepPulser (
  DRepPulsingState (..),
  DRepPulser (..),
  pulseDRepPulsingState,
  completeDRepPulsingState,
  extractDRepPulsingState,
  finishDRepPulser,
  computeDRepDistr,
  psDRepDistrG,
  PulsingSnapshot (..),
  psProposalsL,
  psDRepDistrL,
  psDRepStateL,
  psPoolDistrL,
  RunConwayRatify (..),
) where

import Cardano.Ledger.BaseTypes (EpochNo (..), Globals (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  Interns,
  ToCBOR (..),
  decNoShareCBOR,
  decodeMap,
  decodeStrictSeq,
  interns,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.CertState (CommitteeState)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Era (ConwayRATIFY)
import Cardano.Ledger.Conway.Governance.Internal
import Cardano.Ledger.Conway.Governance.Procedures (GovActionState)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.PoolParams (PoolParams)
import Cardano.Ledger.UMap
import qualified Cardano.Ledger.UMap as UMap
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (guard)
import Control.Monad.Trans.Reader (Reader, runReader)
import Control.State.Transition.Extended
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default (Default (..))
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Pulse (Pulsable (..), pulse)
import Data.Sequence.Strict (StrictSeq (..))
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..), allNoThunks)

-- | A snapshot of information from the previous epoch stored inside the Pulser.
--   After the pulser completes, but before the epoch turns, this information
--   is store in the 'DRComplete' constructor of the 'DRepPulsingState'
--   These are the values at the start of the current epoch. This allows the API
--   To access these "previous" values, both during and after pulsing.
data PulsingSnapshot era = PulsingSnapshot
  { psProposals :: !(StrictSeq (GovActionState era))
  , psDRepDistr :: !(Map DRep (CompactForm Coin))
  , psDRepState :: !(Map (Credential 'DRepRole) DRepState)
  , psPoolDistr :: Map (KeyHash 'StakePool) (CompactForm Coin)
  }
  deriving (Generic)

psProposalsL :: Lens' (PulsingSnapshot era) (StrictSeq (GovActionState era))
psProposalsL = lens psProposals (\x y -> x {psProposals = y})

psDRepDistrL :: Lens' (PulsingSnapshot era) (Map DRep (CompactForm Coin))
psDRepDistrL = lens psDRepDistr (\x y -> x {psDRepDistr = y})

psDRepStateL ::
  Lens' (PulsingSnapshot era) (Map (Credential 'DRepRole) DRepState)
psDRepStateL = lens psDRepState (\x y -> x {psDRepState = y})

psPoolDistrL ::
  Lens'
    (PulsingSnapshot era)
    (Map (KeyHash 'StakePool) (CompactForm Coin))
psPoolDistrL = lens psPoolDistr (\x y -> x {psPoolDistr = y})

deriving instance EraPParams era => Eq (PulsingSnapshot era)

deriving instance EraPParams era => Show (PulsingSnapshot era)

instance EraPParams era => NFData (PulsingSnapshot era)

instance EraPParams era => NoThunks (PulsingSnapshot era)

toPulsingSnapshotsPairs :: (KeyValue e a, EraPParams era) => PulsingSnapshot era -> [a]
toPulsingSnapshotsPairs gas@(PulsingSnapshot _ _ _ _) =
  let (PulsingSnapshot {..}) = gas
   in [ "psProposals" .= psProposals
      , "psDRepDistr" .= psDRepDistr
      , "psDRepState" .= psDRepState
      , "psPoolDistr" .= psPoolDistr
      ]

instance EraPParams era => ToJSON (PulsingSnapshot era) where
  toJSON = object . toPulsingSnapshotsPairs
  toEncoding = pairs . mconcat . toPulsingSnapshotsPairs

instance Default (PulsingSnapshot era) where
  def = PulsingSnapshot mempty def def def

instance EraPParams era => EncCBOR (PulsingSnapshot era) where
  encCBOR PulsingSnapshot {..} =
    encode $
      Rec PulsingSnapshot
        !> To psProposals
        !> To psDRepDistr
        !> To psDRepState
        !> To psPoolDistr

instance EraPParams era => DecShareCBOR (PulsingSnapshot era) where
  type
    Share (PulsingSnapshot era) =
      ( Interns (Credential 'Staking)
      , Interns (KeyHash 'StakePool)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  decShareCBOR is@(_, ks, cd, _) =
    decode $
      RecD PulsingSnapshot
        <! D (decodeStrictSeq (decShareCBOR is))
        <! D (decodeMap (decShareCBOR cd) decCBOR)
        <! D (decodeMap (interns cd <$> decCBOR) decCBOR) --TODO: implement sharing for DRepState
        <! D (decodeMap (interns ks <$> decCBOR) decCBOR)

instance EraPParams era => DecCBOR (PulsingSnapshot era) where
  decCBOR = decNoShareCBOR

instance EraPParams era => ToCBOR (PulsingSnapshot era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (PulsingSnapshot era) where
  fromCBOR = fromEraCBOR @era

-- | We iterate over a pulse-sized chunk of the UMap.
--
-- For each staking credential in the chunk that has delegated to a DRep, add
-- the stake distribution, rewards, and proposal deposits for that credential to
-- the DRep distribution, if the DRep is a DRepCredential (also, AlwaysAbstain
-- or AlwaysNoConfidence) and a member of the registered DReps. If the
-- DRepCredential is not a member of the registered DReps, ignore and skip that
-- DRep.
--
-- For each staking credential in the chunk that has delegated to an SPO,
-- add only the proposal deposits for that credential to the stake pool
-- distribution, since the rewards and stake are already added to it by the
-- SNAP rule.
--
-- Give or take, this operation has roughly
-- @
--   O (a * (log(b) + log(c) + log(d) + log(e) + log(f)))
-- @
-- complexity, where,
--   (a) is the size of the chunk of the UMap, which is the pulse-size, iterate over
--   (b) is the size of the StakeDistr, lookup
--   (c) is the size of the DRepDistr, insertWith
--   (d) is the size of the dpProposalDeposits, lookup
--   (e) is the size of the registered DReps, lookup
--   (f) is the size of the PoolDistr, insert
computeDRepDistr ::
  Map (Credential 'Staking) (CompactForm Coin) ->
  Map (Credential 'DRepRole) DRepState ->
  Map (Credential 'Staking) (CompactForm Coin) ->
  PoolDistr ->
  Map DRep (CompactForm Coin) ->
  Map (Credential 'Staking) UMElem ->
  (Map DRep (CompactForm Coin), PoolDistr)
computeDRepDistr stakeDistr regDReps proposalDeposits poolDistr dRepDistr uMapChunk =
  Map.foldlWithKey' go (dRepDistr, poolDistr) uMapChunk
  where
    go (!drepAccum, !poolAccum) stakeCred umElem =
      let stake = fromMaybe (CompactCoin 0) $ Map.lookup stakeCred stakeDistr
          proposalDeposit = fromMaybe (CompactCoin 0) $ Map.lookup stakeCred proposalDeposits
          stakeAndDeposits = addCompact stake proposalDeposit
       in case umElemDelegations umElem of
            Nothing -> (drepAccum, poolAccum)
            Just (RewardDelegationSPO spo _r) ->
              ( drepAccum
              , addToPoolDistr spo proposalDeposit poolAccum
              )
            Just (RewardDelegationDRep drep r) ->
              ( addToDRepDistr drep (addCompact stakeAndDeposits r) drepAccum
              , poolAccum
              )
            Just (RewardDelegationBoth spo drep r) ->
              ( addToDRepDistr drep (addCompact stakeAndDeposits r) drepAccum
              , addToPoolDistr spo proposalDeposit poolAccum
              )
    addToPoolDistr spo proposalDeposit distr = fromMaybe distr $ do
      guard (proposalDeposit /= mempty)
      ips <- Map.lookup spo $ distr ^. poolDistrDistrL
      pure $
        distr
          & poolDistrDistrL %~ Map.insert spo (ips & individualTotalPoolStakeL <>~ proposalDeposit)
          & poolDistrTotalL <>~ proposalDeposit
    addToDRepDistr drep ccoin distr =
      let updatedDistr = Map.insertWith addCompact drep ccoin distr
       in case drep of
            DRepAlwaysAbstain -> updatedDistr
            DRepAlwaysNoConfidence -> updatedDistr
            DRepCredential cred
              | Map.member cred regDReps -> updatedDistr
              | otherwise -> distr

-- | The type of a Pulser which uses 'computeDRepDistr' as its underlying
-- function. Note that we use two type equality (~) constraints to fix both
-- the monad 'm' and the 'ans' type, to the context where we will use the
-- type as a Pulser. The type DRepPulser must have 'm' and 'ans' as its last
-- two parameters so we can make a Pulsable instance. We will always use this
-- instantiation (DRepPulser era Identity (RatifyState era))
data DRepPulser era (m :: Type -> Type) ans where
  DRepPulser ::
    forall era ans m.
    (ans ~ RatifyState era, m ~ Identity, RunConwayRatify era) =>
    { dpPulseSize :: !Int
    -- ^ How many elements of 'dpUMap' to consume each pulse.
    , dpUMap :: !UMap
    -- ^ Snapshot containing the mapping of stake credentials to DReps, Pools and Rewards.
    , dpIndex :: !Int
    -- ^ The index of the iterator over `dpUMap`. Grows with each pulse.
    , dpStakeDistr :: !(Map (Credential 'Staking) (CompactForm Coin))
    -- ^ Snapshot of the stake distr (comes from the IncrementalStake)
    , dpStakePoolDistr :: PoolDistr
    -- ^ Snapshot of the pool distr. Lazy on purpose: See `ssStakeMarkPoolDistr` and ADR-7
    -- for explanation.
    , dpDRepDistr :: !(Map DRep (CompactForm Coin))
    -- ^ The partial result that grows with each pulse. The purpose of the pulsing.
    , dpDRepState :: !(Map (Credential 'DRepRole) DRepState)
    -- ^ Snapshot of registered DRep credentials
    , dpCurrentEpoch :: !EpochNo
    -- ^ Snapshot of the EpochNo this pulser will complete in.
    , dpCommitteeState :: !(CommitteeState era)
    -- ^ Snapshot of the CommitteeState
    , dpEnactState :: !(EnactState era)
    -- ^ Snapshot of the EnactState, Used to build the Env of the RATIFY rule
    , dpProposals :: !(StrictSeq (GovActionState era))
    -- ^ Snapshot of the proposals. This is the Signal for the RATIFY rule
    , dpProposalDeposits :: !(Map (Credential 'Staking) (CompactForm Coin))
    -- ^ Snapshot of the proposal-deposits per reward-account-staking-credential
    , dpGlobals :: !Globals
    , dpPoolParams :: !(Map (KeyHash 'StakePool) PoolParams)
    -- ^ Snapshot of the parameters of stake pools -
    --   this is needed to get the reward account for SPO vote calculation
    } ->
    DRepPulser era m ans

instance EraPParams era => Eq (DRepPulser era Identity (RatifyState era)) where
  x == y = finishDRepPulser (DRPulsing x) == finishDRepPulser (DRPulsing y)

instance Pulsable (DRepPulser era) where
  done DRepPulser {dpUMap, dpIndex} = dpIndex >= Map.size (UMap.umElems dpUMap)

  current x@(DRepPulser {}) = snd $ finishDRepPulser (DRPulsing x)

  pulseM pulser@(DRepPulser {..})
    | done pulser = pure pulser {dpIndex = 0}
    | otherwise =
        let !chunk = Map.take dpPulseSize $ Map.drop dpIndex $ UMap.umElems dpUMap
            (dRepDistr, poolDistr) =
              computeDRepDistr dpStakeDistr dpDRepState dpProposalDeposits dpStakePoolDistr dpDRepDistr chunk
         in pure $
              pulser
                { dpIndex = dpIndex + dpPulseSize
                , dpDRepDistr = dRepDistr
                , dpStakePoolDistr = poolDistr
                }

  completeM x@(DRepPulser {}) = pure (snd $ finishDRepPulser @era (DRPulsing x))

deriving instance (EraPParams era, Show ans) => Show (DRepPulser era m ans)

instance EraPParams era => NoThunks (DRepPulser era Identity (RatifyState era)) where
  showTypeOf _ = "DRepPulser"
  wNoThunks ctxt drp@(DRepPulser _ _ _ _ _ _ _ _ _ _ _ _ _ _) =
    allNoThunks
      [ noThunks ctxt (dpPulseSize drp)
      , noThunks ctxt (dpUMap drp)
      , noThunks ctxt (dpIndex drp)
      , noThunks ctxt (dpStakeDistr drp)
      , -- dpStakePoolDistr is allowed to have thunks
        noThunks ctxt (dpDRepDistr drp)
      , noThunks ctxt (dpDRepState drp)
      , noThunks ctxt (dpCurrentEpoch drp)
      , noThunks ctxt (dpCommitteeState drp)
      , noThunks ctxt (dpEnactState drp)
      , noThunks ctxt (dpProposals drp)
      , noThunks ctxt (dpProposalDeposits drp)
      , noThunks ctxt (dpGlobals drp)
      , noThunks ctxt (dpPoolParams drp)
      ]

instance EraPParams era => NFData (DRepPulser era Identity (RatifyState era)) where
  rnf (DRepPulser n um bal stake pool drep dstate ep cs es as pds gs poolps) =
    n `deepseq`
      um `deepseq`
        bal `deepseq`
          stake `deepseq`
            pool `deepseq`
              drep `deepseq`
                dstate `deepseq`
                  ep `deepseq`
                    cs `deepseq`
                      es `deepseq`
                        as `deepseq`
                          pds `deepseq`
                            gs `deepseq`
                              rnf poolps

class
  ( STS (ConwayRATIFY era)
  , Signal (ConwayRATIFY era) ~ RatifySignal era
  , BaseM (ConwayRATIFY era) ~ Reader Globals
  , Environment (ConwayRATIFY era) ~ RatifyEnv era
  , State (ConwayRATIFY era) ~ RatifyState era
  , PredicateFailure (ConwayRATIFY era) ~ Void
  ) =>
  RunConwayRatify era
  where
  runConwayRatify ::
    Globals -> RatifyEnv era -> RatifyState era -> RatifySignal era -> RatifyState era
  runConwayRatify globals ratifyEnv ratifyState (RatifySignal ratifySig) =
    let ratifyResult =
          runReader
            ( applySTS @(ConwayRATIFY era) $
                TRC (ratifyEnv, ratifyState, RatifySignal $ reorderActions ratifySig)
            )
            globals
     in case ratifyResult of
          Left (x :| _) -> absurd x
          Right ratifyState' -> ratifyState'

finishDRepPulser :: forall era. DRepPulsingState era -> (PulsingSnapshot era, RatifyState era)
finishDRepPulser (DRComplete snap ratifyState) = (snap, ratifyState)
finishDRepPulser (DRPulsing (DRepPulser {..})) =
  ( PulsingSnapshot
      dpProposals
      finalDRepDistr
      dpDRepState
      (Map.map individualTotalPoolStake $ unPoolDistr finalStakePoolDistr)
  , ratifyState'
  )
  where
    !leftOver = Map.drop dpIndex $ umElems dpUMap
    (finalDRepDistr, finalStakePoolDistr) =
      computeDRepDistr dpStakeDistr dpDRepState dpProposalDeposits dpStakePoolDistr dpDRepDistr leftOver
    !ratifyEnv =
      RatifyEnv
        { reStakeDistr = dpStakeDistr
        , reStakePoolDistr = finalStakePoolDistr
        , reDRepDistr = finalDRepDistr
        , reDRepState = dpDRepState
        , reCurrentEpoch = dpCurrentEpoch
        , reCommitteeState = dpCommitteeState
        , reDelegatees = dRepMap dpUMap
        , rePoolParams = dpPoolParams
        }
    !ratifySig = RatifySignal dpProposals
    !ratifyState =
      RatifyState
        { rsEnactState = dpEnactState
        , rsEnacted = mempty
        , rsExpired = mempty
        , rsDelayed = False
        }
    !ratifyState' = runConwayRatify dpGlobals ratifyEnv ratifyState ratifySig

-- ===========================================================
-- The State which is stored in ConwayGovState
-- ===========================================================

data DRepPulsingState era
  = DRPulsing !(DRepPulser era Identity (RatifyState era))
  | DRComplete
      !(PulsingSnapshot era)
      !(RatifyState era)
  deriving (Generic, NoThunks, NFData)

-- | This is potentially an expensive getter. Make sure not to use it in the first 80% of
-- the epoch.
psDRepDistrG ::
  SimpleGetter (DRepPulsingState era) (Map DRep (CompactForm Coin))
psDRepDistrG = to get
  where
    get (DRComplete x _) = psDRepDistr x
    get x = psDRepDistr . fst $ finishDRepPulser x

instance EraPParams era => Eq (DRepPulsingState era) where
  x == y = finishDRepPulser x == finishDRepPulser y

instance EraPParams era => Show (DRepPulsingState era) where
  show (DRComplete x m) = "(DRComplete " ++ show x ++ " " ++ show m ++ ")"
  show x = show (uncurry DRComplete (finishDRepPulser x))

instance EraPParams era => EncCBOR (DRepPulsingState era) where
  encCBOR (DRComplete x y) = encode (Rec DRComplete !> To x !> To y)
  encCBOR x@(DRPulsing (DRepPulser {})) = encode (Rec DRComplete !> To snap !> To ratstate)
    where
      (snap, ratstate) = finishDRepPulser x

instance EraPParams era => DecShareCBOR (DRepPulsingState era) where
  type
    Share (DRepPulsingState era) =
      ( Interns (Credential 'Staking)
      , Interns (KeyHash 'StakePool)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  decShareCBOR is =
    decode $
      RecD DRComplete
        <! From
        <! D (decShareCBOR is)

instance EraPParams era => DecCBOR (DRepPulsingState era) where
  decCBOR = decode (RecD DRComplete <! From <! From)

-- =====================================
-- High level operations of DRepDistr

pulseDRepPulsingState :: DRepPulsingState era -> DRepPulsingState era
pulseDRepPulsingState x@(DRComplete _ _) = x
pulseDRepPulsingState (DRPulsing x@(DRepPulser {})) =
  let x2 = pulse x
   in if done x2
        then uncurry DRComplete (finishDRepPulser (DRPulsing x2))
        else DRPulsing x2

completeDRepPulsingState :: DRepPulsingState era -> DRepPulsingState era
completeDRepPulsingState x@(DRPulsing _) = uncurry DRComplete (finishDRepPulser x)
completeDRepPulsingState x@(DRComplete {}) = x

extractDRepPulsingState :: DRepPulsingState era -> RatifyState era
extractDRepPulsingState x@(DRPulsing _) = snd (finishDRepPulser x)
extractDRepPulsingState (DRComplete _ x) = x
