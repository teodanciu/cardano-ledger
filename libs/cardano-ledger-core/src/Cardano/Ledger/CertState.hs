{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.CertState (
  EraCertState (..),
  CommitteeAuthorization (..),
  DState (..),
  PState (..),
  VState (..),
  InstantaneousRewards (..),
  FutureGenDeleg (..),
  Anchor (..),
  DRepState (..),
  DRep (..),
  CommitteeState (..),
  authorizedHotCommitteeCredentials,
  AnchorData,
  lookupDepositDState,
  lookupRewardDState,
  rewards,
  delegations,
  ptrsMap,
  payPoolDeposit,
  refundPoolDeposit,
  Obligations (..),
  sumObligation,
  -- Lenses
  dsUnifiedL,
  dsGenDelegsL,
  dsIRewardsL,
  dsFutureGenDelegsL,
  psStakePoolParamsL,
  psFutureStakePoolParamsL,
  psRetiringL,
  psDepositsL,
  vsDRepsL,
  vsCommitteeStateL,
  vsNumDormantEpochsL,
  vsActualDRepExpiry,
  csCommitteeCredsL,
  lookupDepositVState,
)
where

import Cardano.Ledger.BaseTypes (Anchor (..), AnchorData, StrictMaybe, binOpEpochNo)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  Interns,
  ToCBOR (..),
  decNoShareCBOR,
  decSharePlusCBOR,
  decSharePlusLensCBOR,
  decodeMap,
  decodeRecordNamed,
  decodeRecordNamedT,
  encodeListLen,
  interns,
  internsFromSet,
  toMemptyLens,
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), Ptr, StakeCredential)
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (GenDelegPair (..), GenDelegs (..))
import Cardano.Ledger.PoolParams (PoolParams)
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import Cardano.Ledger.UMap (RDPair (..), UMap (UMap), UView (RewDepUView, SPoolUView))
import qualified Cardano.Ledger.UMap as UM
import Control.DeepSeq (NFData (..))
import Control.Monad.Trans
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default (Default (def))
import qualified Data.Foldable as F
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.), _1)
import NoThunks.Class (NoThunks (..))

-- ======================================

data FutureGenDeleg = FutureGenDeleg
  { fGenDelegSlot :: !SlotNo
  , fGenDelegGenKeyHash :: !(KeyHash 'Genesis)
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks FutureGenDeleg

instance NFData FutureGenDeleg

instance EncCBOR FutureGenDeleg where
  encCBOR (FutureGenDeleg a b) =
    encodeListLen 2 <> encCBOR a <> encCBOR b

instance DecCBOR FutureGenDeleg where
  decCBOR =
    decodeRecordNamed "FutureGenDeleg" (const 2) $
      FutureGenDeleg <$> decCBOR <*> decCBOR

instance ToJSON FutureGenDeleg where
  toJSON fGenDeleg =
    object
      [ "fGenDelegSlot" .= fGenDelegSlot fGenDeleg
      , "fGenDelegGenKeyHash" .= fGenDelegGenKeyHash fGenDeleg
      ]

-- | InstantaneousRewards captures the pending changes to the ledger
-- state caused by MIR certificates. It consists of two mappings,
-- the rewards which will be paid out from the reserves and the rewards
-- which will be paid out from the treasury. It also consists of
-- two coin values which represent the transfer of coins from
-- one pot to the other pot.
-- NOTE that the following property should always hold:
--   deltaReserves + deltaTreasury = 0
data InstantaneousRewards = InstantaneousRewards
  { iRReserves :: !(Map (Credential 'Staking) Coin)
  , iRTreasury :: !(Map (Credential 'Staking) Coin)
  , deltaReserves :: !DeltaCoin
  , deltaTreasury :: !DeltaCoin
  }
  deriving (Show, Eq, Generic)

instance NoThunks InstantaneousRewards

instance NFData InstantaneousRewards

instance ToJSON InstantaneousRewards where
  toJSON = object . toInstantaneousRewardsPair
  toEncoding = pairs . mconcat . toInstantaneousRewardsPair

toInstantaneousRewardsPair :: KeyValue e a => InstantaneousRewards -> [a]
toInstantaneousRewardsPair InstantaneousRewards {..} =
  [ "iRReserves" .= iRReserves
  , "iRTreasury" .= iRTreasury
  , "deltaReserves" .= deltaReserves
  , "deltaTreasury" .= deltaTreasury
  ]

-- | The state used by the DELEG rule, which roughly tracks stake
-- delegation and some governance features.
data DState era = DState
  { dsUnified :: !UMap
  -- ^ Unified Reward Maps. This contains the reward map (which is the source
  -- of truth regarding the registered stake credentials, the deposit map,
  -- the delegation map, and the stake credential pointer map.
  , dsFutureGenDelegs :: !(Map FutureGenDeleg GenDelegPair)
  -- ^ Future genesis key delegations
  , dsGenDelegs :: !GenDelegs
  -- ^ Genesis key delegations
  , dsIRewards :: !InstantaneousRewards
  -- ^ Instantaneous Rewards
  }
  deriving (Show, Eq, Generic)

instance NoThunks (DState era)

instance NFData (DState era)

instance Era era => EncCBOR (DState era) where
  encCBOR (DState unified fgs gs ir) =
    encodeListLen 4
      <> encCBOR unified
      <> encCBOR fgs
      <> encCBOR gs
      <> encCBOR ir

instance DecShareCBOR (DState era) where
  type
    Share (DState era) =
      (Interns (Credential 'Staking), Interns (KeyHash 'StakePool), Interns (Credential 'DRepRole))
  decSharePlusCBOR =
    decodeRecordNamedT "DState" (const 4) $ do
      unified <- decSharePlusCBOR
      fgs <- lift decCBOR
      gs <- lift decCBOR
      ir <- decSharePlusLensCBOR _1
      pure $ DState unified fgs gs ir

instance ToJSON (DState era) where
  toJSON = object . toDStatePair
  toEncoding = pairs . mconcat . toDStatePair

toDStatePair :: KeyValue e a => DState era -> [a]
toDStatePair DState {..} =
  [ "unified" .= dsUnified
  , "fGenDelegs" .= Map.toList dsFutureGenDelegs
  , "genDelegs" .= dsGenDelegs
  , "irwd" .= dsIRewards
  ]

-- | Function that looks up the deposit for currently delegated staking credential
lookupDepositDState :: DState era -> (StakeCredential -> Maybe Coin)
lookupDepositDState dstate =
  let currentRewardDeposits = RewDepUView $ dsUnified dstate
   in \k -> do
        RDPair _ deposit <- UM.lookup k currentRewardDeposits
        Just $! fromCompact deposit

-- | Function that looks up curret reward for the delegated staking credential.
lookupRewardDState :: DState era -> (StakeCredential -> Maybe Coin)
lookupRewardDState dstate =
  let currentRewardDeposits = RewDepUView $ dsUnified dstate
   in \k -> do
        RDPair reward _ <- UM.lookup k currentRewardDeposits
        Just $! fromCompact reward

-- | The state used by the POOL rule, which tracks stake pool information.
data PState era = PState
  { psStakePoolParams :: !(Map (KeyHash 'StakePool) PoolParams)
  -- ^ The stake pool parameters.
  , psFutureStakePoolParams :: !(Map (KeyHash 'StakePool) PoolParams)
  -- ^ The future stake pool parameters.
  -- Changes to existing stake pool parameters are staged in order
  -- to give delegators time to react to changes.
  -- See section 11.2, "Example Illustration of the Reward Cycle",
  -- of the Shelley Ledger Specification for a sequence diagram.
  , psRetiring :: !(Map (KeyHash 'StakePool) EpochNo)
  -- ^ A map of retiring stake pools to the epoch when they retire.
  , psDeposits :: !(Map (KeyHash 'StakePool) Coin)
  -- ^ A map of the deposits for each pool
  }
  deriving (Show, Eq, Generic)

instance NoThunks (PState era)

instance NFData (PState era)

instance Era era => EncCBOR (PState era) where
  encCBOR (PState a b c d) =
    encodeListLen 4 <> encCBOR a <> encCBOR b <> encCBOR c <> encCBOR d

instance DecShareCBOR (PState era) where
  type Share (PState era) = Interns (KeyHash 'StakePool)
  decSharePlusCBOR = decodeRecordNamedT "PState" (const 4) $ do
    psStakePoolParams <- decSharePlusLensCBOR (toMemptyLens _1 id)
    psFutureStakePoolParams <- decSharePlusLensCBOR (toMemptyLens _1 id)
    psRetiring <- decSharePlusLensCBOR (toMemptyLens _1 id)
    psDeposits <- decSharePlusLensCBOR (toMemptyLens _1 id)
    pure PState {psStakePoolParams, psFutureStakePoolParams, psRetiring, psDeposits}

instance (Era era, DecShareCBOR (PState era)) => DecCBOR (PState era) where
  decCBOR = decNoShareCBOR

instance ToJSON (PState era) where
  toJSON = object . toPStatePair
  toEncoding = pairs . mconcat . toPStatePair

toPStatePair :: KeyValue e a => PState era -> [a]
toPStatePair PState {..} =
  [ "stakePoolParams" .= psStakePoolParams
  , "futureStakePoolParams" .= psFutureStakePoolParams
  , "retiring" .= psRetiring
  , "deposits" .= psDeposits
  ]

data CommitteeAuthorization
  = -- | Member authorized with a Hot credential acting on behalf of their Cold credential
    CommitteeHotCredential !(Credential 'HotCommitteeRole)
  | -- | Member resigned with a potential explanation in Anchor
    CommitteeMemberResigned !(StrictMaybe Anchor)
  deriving (Eq, Ord, Show, Generic)

instance NoThunks CommitteeAuthorization
instance NFData CommitteeAuthorization
instance ToJSON CommitteeAuthorization

instance EncCBOR CommitteeAuthorization where
  encCBOR =
    encode . \case
      CommitteeHotCredential cred -> Sum CommitteeHotCredential 0 !> To cred
      CommitteeMemberResigned anchor -> Sum CommitteeMemberResigned 1 !> To anchor

instance DecCBOR CommitteeAuthorization where
  decCBOR =
    decode $ Summands "CommitteeAuthorization" $ \case
      0 -> SumD CommitteeHotCredential <! From
      1 -> SumD CommitteeMemberResigned <! From
      k -> Invalid k

newtype CommitteeState era = CommitteeState
  { csCommitteeCreds :: Map (Credential 'ColdCommitteeRole) CommitteeAuthorization
  }
  deriving (Eq, Ord, Show, Generic, EncCBOR, NFData, Default, NoThunks)

instance ToJSON (CommitteeState era)

-- | Extract all unique hot credential authorizations for the current committee.  Note
-- that there is no unique mapping from Hot to Cold credential, therefore we produce a
-- Set, instead of a Map.
authorizedHotCommitteeCredentials :: CommitteeState era -> Set.Set (Credential 'HotCommitteeRole)
authorizedHotCommitteeCredentials CommitteeState {csCommitteeCreds} =
  let toHotCredSet acc = \case
        CommitteeHotCredential hotCred -> Set.insert hotCred acc
        CommitteeMemberResigned {} -> acc
   in F.foldl' toHotCredSet Set.empty csCommitteeCreds

instance Era era => DecShareCBOR (CommitteeState era) where
  type Share (CommitteeState era) = Interns (Credential 'HotCommitteeRole)
  getShare = internsFromSet . authorizedHotCommitteeCredentials
  decShareCBOR _ = CommitteeState <$> decCBOR

instance Era era => DecCBOR (CommitteeState era) where
  decCBOR = decNoShareCBOR

instance Era era => ToCBOR (CommitteeState era) where
  toCBOR = toEraCBOR @era

-- | The state that tracks the voting entities (DReps and Constitutional Committee
-- members). In the formal ledger specification this type is called @GState@
data VState era = VState
  { vsDReps :: !(Map (Credential 'DRepRole) DRepState)
  , vsCommitteeState :: !(CommitteeState era)
  , vsNumDormantEpochs :: !EpochNo
  -- ^ Number of contiguous epochs in which there are exactly zero
  -- active governance proposals to vote on. It is incremented in every
  -- EPOCH rule if the number of active governance proposals to vote on
  -- continues to be zero. It is reset to zero when a new governance
  -- action is successfully proposed. We need this counter in order to
  -- bump DRep expiries through dormant periods when DReps do not have
  -- an opportunity to vote on anything.
  }
  deriving (Show, Eq, Generic)

-- | Function that looks up the deposit for currently registered DRep
lookupDepositVState :: VState era -> Credential 'DRepRole -> Maybe Coin
lookupDepositVState vstate = fmap drepDeposit . flip Map.lookup (vstate ^. vsDRepsL)

instance Default (VState era) where
  def = VState def def (EpochNo 0)

instance NoThunks (VState era)

instance NFData (VState era)

instance Era era => DecShareCBOR (VState era) where
  type
    Share (VState era) =
      ( Interns (Credential 'Staking)
      , Interns (Credential 'DRepRole)
      , Interns (Credential 'HotCommitteeRole)
      )
  getShare VState {vsDReps, vsCommitteeState} =
    (internsFromSet (foldMap drepDelegs vsDReps), fst (getShare vsDReps), getShare vsCommitteeState)
  decShareCBOR (cs, cd, _) =
    decode $
      RecD VState
        <! D (decodeMap (interns cd <$> decCBOR) (decShareCBOR cs))
        <! D decNoShareCBOR
        <! From

instance Era era => DecCBOR (VState era) where
  decCBOR = decNoShareCBOR

instance Era era => EncCBOR (VState era) where
  encCBOR VState {..} =
    encode $
      Rec (VState @era)
        !> To vsDReps
        !> To vsCommitteeState
        !> To vsNumDormantEpochs

instance ToJSON (VState era) where
  toJSON = object . toVStatePair
  toEncoding = pairs . mconcat . toVStatePair

toVStatePair :: KeyValue e a => VState era -> [a]
toVStatePair vs@(VState _ _ _) =
  let VState {..} = vs
   in [ "dreps" .= vsDReps
      , "committeeState" .= vsCommitteeState
      , "numDormantEpochs" .= vsNumDormantEpochs
      ]

-- | The state associated with the DELPL rule, which combines the DELEG rule
-- and the POOL rule.
class
  ( Era era
  , ToJSON (CertState era)
  , EncCBOR (CertState era)
  , DecShareCBOR (CertState era)
  , Share (CertState era)
      ~ ( Interns (Credential 'Staking)
        , Interns (KeyHash 'StakePool)
        , Interns (Credential 'DRepRole)
        , Interns (Credential 'HotCommitteeRole)
        )
  , Default (CertState era)
  , NoThunks (CertState era)
  , NFData (CertState era)
  , Show (CertState era)
  , Eq (CertState era)
  , Generic (CertState era)
  ) =>
  EraCertState era
  where
  type CertState era = (r :: Type) | r -> era

  mkCertState :: VState era -> PState era -> DState era -> CertState era

  upgradeCertState :: EraCertState (PreviousEra era) => CertState (PreviousEra era) -> CertState era

  certDStateL :: Lens' (CertState era) (DState era)

  certPStateL :: Lens' (CertState era) (PState era)

  certVStateL :: Lens' (CertState era) (VState era)

  -- | Calculate total possible refunds in the system that are related to certificates
  --
  -- There is an invariant that the sum of all the fields should be the same as the
  -- utxosDeposited field of the UTxOState. Note that this does not depend upon the current
  -- values of the Key and Pool deposits of the PParams.
  obligationCertState :: CertState era -> Obligations

  -- | Compute the total deposits from the Certs of a TxBody.
  --
  -- This is the contribution of a TxBody towards the deposit pot (utxosDeposit field of
  -- the UTxOState) of the system
  certsTotalDepositsTxBody :: EraTxBody era => PParams era -> CertState era -> TxBody era -> Coin

  -- | Compute the total refunds from the Certs of a TxBody.
  --
  -- This is the contribution of a TxBody towards the total 'Obligations' of the system
  -- See `Obligations` and `obligationCertState` for more information.
  certsTotalRefundsTxBody :: EraTxBody era => PParams era -> CertState era -> TxBody era -> Coin

instance EncCBOR InstantaneousRewards where
  encCBOR (InstantaneousRewards irR irT dR dT) =
    encodeListLen 4 <> encCBOR irR <> encCBOR irT <> encCBOR dR <> encCBOR dT

instance DecShareCBOR InstantaneousRewards where
  type Share InstantaneousRewards = Interns (Credential 'Staking)
  decSharePlusCBOR =
    decodeRecordNamedT "InstantaneousRewards" (const 4) $ do
      irR <- decSharePlusLensCBOR (toMemptyLens _1 id)
      irT <- decSharePlusLensCBOR (toMemptyLens _1 id)
      dR <- lift decCBOR
      dT <- lift decCBOR
      pure $ InstantaneousRewards irR irT dR dT

instance Default InstantaneousRewards where
  def = InstantaneousRewards Map.empty Map.empty mempty mempty

instance Default (DState era) where
  def =
    DState
      UM.empty
      Map.empty
      (GenDelegs Map.empty)
      def

instance Default (PState era) where
  def =
    PState Map.empty Map.empty Map.empty Map.empty

rewards :: DState era -> UView (Credential 'Staking) RDPair
rewards = RewDepUView . dsUnified

delegations ::
  DState era ->
  UView (Credential 'Staking) (KeyHash 'StakePool)
delegations = SPoolUView . dsUnified

-- | get the actual ptrs map, we don't need a view
ptrsMap :: DState era -> Map Ptr (Credential 'Staking)
ptrsMap (DState {dsUnified = UMap _ ptrmap}) = ptrmap

-- ==========================================================
-- Functions that handle Deposits

-- | One only pays a deposit on the initial pool registration. So return the
--   the Deposits unchanged if the keyhash already exists. There are legal
--   situations where a pool may be registered multiple times.
payPoolDeposit ::
  EraPParams era =>
  KeyHash 'StakePool ->
  PParams era ->
  PState era ->
  PState era
payPoolDeposit keyhash pp pstate = pstate {psDeposits = newpool}
  where
    pool = psDeposits pstate
    !deposit = pp ^. ppPoolDepositL
    newpool
      | Map.notMember keyhash pool = Map.insert keyhash deposit pool
      | otherwise = pool

refundPoolDeposit :: KeyHash 'StakePool -> PState era -> (Coin, PState era)
refundPoolDeposit keyhash pstate = (coin, pstate {psDeposits = newpool})
  where
    pool = psDeposits pstate
    (coin, newpool) = case Map.lookup keyhash pool of
      Just c -> (c, Map.delete keyhash pool)
      Nothing -> (mempty, pool)

-- | A composite of all the Deposits the system is obligated to eventually pay back.
data Obligations = Obligations
  { oblStake :: !Coin
  , oblPool :: !Coin
  , oblDRep :: !Coin
  , oblProposal :: !Coin
  }
  deriving (Eq, Ord, Generic)

instance NFData Obligations

sumObligation :: Obligations -> Coin
sumObligation x = oblStake x <> oblPool x <> oblDRep x <> oblProposal x

instance Semigroup Obligations where
  x <> y =
    Obligations
      { oblStake = oblStake x <> oblStake y
      , oblPool = oblPool x <> oblPool y
      , oblDRep = oblDRep x <> oblDRep y
      , oblProposal = oblProposal x <> oblProposal y
      }

instance Monoid Obligations where
  mempty = Obligations {oblStake = Coin 0, oblPool = Coin 0, oblDRep = Coin 0, oblProposal = Coin 0}

instance Show Obligations where
  show x =
    unlines
      [ "Total Obligations = " ++ show (sumObligation x)
      , "   Stake deposits = " ++ show (oblStake x)
      , "   Pool deposits = " ++ show (oblPool x)
      , "   DRep deposits = " ++ show (oblDRep x)
      , "   Proposal deposits = " ++ show (oblProposal x)
      ]

-- =======================================================
-- Lenses for CertState and its subsidiary types

-- ===================================
-- DState

dsUnifiedL :: Lens' (DState era) UMap
dsUnifiedL = lens dsUnified (\ds u -> ds {dsUnified = u})

dsGenDelegsL :: Lens' (DState era) GenDelegs
dsGenDelegsL = lens dsGenDelegs (\ds u -> ds {dsGenDelegs = u})

dsIRewardsL :: Lens' (DState era) InstantaneousRewards
dsIRewardsL = lens dsIRewards (\ds u -> ds {dsIRewards = u})

dsFutureGenDelegsL ::
  Lens' (DState era) (Map FutureGenDeleg GenDelegPair)
dsFutureGenDelegsL = lens dsFutureGenDelegs (\ds u -> ds {dsFutureGenDelegs = u})

-- ===================================
-- PState

psStakePoolParamsL :: Lens' (PState era) (Map (KeyHash 'StakePool) PoolParams)
psStakePoolParamsL = lens psStakePoolParams (\ds u -> ds {psStakePoolParams = u})

psFutureStakePoolParamsL :: Lens' (PState era) (Map (KeyHash 'StakePool) PoolParams)
psFutureStakePoolParamsL = lens psFutureStakePoolParams (\ds u -> ds {psFutureStakePoolParams = u})

psRetiringL :: Lens' (PState era) (Map (KeyHash 'StakePool) EpochNo)
psRetiringL = lens psRetiring (\ds u -> ds {psRetiring = u})

psDepositsL :: Lens' (PState era) (Map (KeyHash 'StakePool) Coin)
psDepositsL = lens psDeposits (\ds u -> ds {psDeposits = u})

-- ===================================
-- VState

vsDRepsL :: Lens' (VState era) (Map (Credential 'DRepRole) DRepState)
vsDRepsL = lens vsDReps (\vs u -> vs {vsDReps = u})

vsCommitteeStateL :: Lens' (VState era) (CommitteeState era)
vsCommitteeStateL = lens vsCommitteeState (\vs u -> vs {vsCommitteeState = u})

vsNumDormantEpochsL :: Lens' (VState era) EpochNo
vsNumDormantEpochsL = lens vsNumDormantEpochs (\vs u -> vs {vsNumDormantEpochs = u})

vsActualDRepExpiry :: Credential 'DRepRole -> VState era -> Maybe EpochNo
vsActualDRepExpiry cred vs =
  binOpEpochNo (+) (vsNumDormantEpochs vs) . drepExpiry <$> Map.lookup cred (vsDReps vs)

csCommitteeCredsL ::
  Lens' (CommitteeState era) (Map (Credential 'ColdCommitteeRole) CommitteeAuthorization)
csCommitteeCredsL = lens csCommitteeCreds (\cs u -> cs {csCommitteeCreds = u})
