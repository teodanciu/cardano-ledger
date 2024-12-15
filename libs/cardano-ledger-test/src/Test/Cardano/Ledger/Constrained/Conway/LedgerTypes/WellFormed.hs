{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

#if __GLASGOW_HASKELL__ < 900
{-# OPTIONS_GHC -O0 #-}
#endif

module Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.WellFormed where

import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes hiding (inject)
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway.Rules (GovEnv (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.EpochBoundary (SnapShot (..), SnapShots (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash ()
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.UTxO (UTxO (..))
import Constrained hiding (Value)
import Data.Map (Map)
import Test.Cardano.Ledger.Constrained.Conway ()
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Specs (
  EraSpecLedger (..),
  accountStateSpec,
  aggregateDRep,
  certStateSpec,
  conwayGovStateSpec,
  dstateSpec,
  epochNoSpec,
  epochStateSpec,
  getMarkSnapShot,
  govEnvSpec,
  ledgerStateSpec,
  pstateSpec,
  shelleyGovStateSpec,
  snapShotSpec,
  snapShotsSpec,
  utxoSpec,
  utxoStateSpec,
  vstateSpec,
 )
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.QuickCheck (Arbitrary (..), Gen)

-- ==============================================================
-- Generators for all the types found in the Ledger State.
-- ==============================================================

ppX :: forall era. EraSpecPParams era => Gen (PParams era)
ppX = genFromSpec @ConwayFn @(PParams era) pparamsSpec

acctX :: Gen AccountState
acctX = genFromSpec @ConwayFn @AccountState accountStateSpec

psX :: forall era. EraSpecPParams era => Gen (PState era)
psX = do
  epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
  genFromSpec @ConwayFn @(PState era) (pstateSpec (lit epoch))

dsX :: forall era. EraSpecLedger era ConwayFn => Gen (DState era)
dsX = do
  acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
  drepRoleSet <- genFromSpec @ConwayFn TrueSpec
  pools <-
    genFromSpec @ConwayFn @(Map (KeyHash 'StakePool) PoolParams)
      (hasSize (rangeSize 8 8))
  genFromSpec @ConwayFn @(DState era) (dstateSpec @era (lit drepRoleSet) (lit acct) (lit pools))

vsX :: forall era. EraSpecPParams era => Gen (VState era)
vsX = do
  epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
  delegatees <-
    aggregateDRep
      <$> genFromSpec @ConwayFn -- ensures that each credential delegates to exactly one DRep
        @(Map (Credential 'Staking) DRep)
        TrueSpec
  genFromSpec @ConwayFn @(VState era) (vstateSpec (lit epoch) (lit delegatees))

csX :: forall era. EraSpecLedger era ConwayFn => Gen (CertState era)
csX = do
  acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
  epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
  delegatees <- arbitrary
  genFromSpec @ConwayFn @(CertState era) (certStateSpec (lit delegatees) (lit acct) (lit epoch))

utxoX :: forall era. EraSpecLedger era ConwayFn => Gen (UTxO era)
utxoX = do
  cs <-
    genFromSpec
      @ConwayFn
      @(Map (Credential 'Staking) (KeyHash 'StakePool))
      (hasSize (rangeSize 30 30))
  genFromSpec @ConwayFn @(UTxO era) (utxoSpec @era (lit cs))

utxostateX :: forall era. EraSpecLedger era ConwayFn => PParams era -> Gen (UTxOState era)
utxostateX pp = do
  certstate <- csX @era
  genFromSpec @ConwayFn @(UTxOState era) (utxoStateSpec pp (lit certstate))

govenvX :: PParams ConwayEra -> Gen (GovEnv ConwayEra)
govenvX pp = genFromSpec @ConwayFn @(GovEnv ConwayEra) (govEnvSpec pp)

conwaygovX :: PParams ConwayEra -> Gen (ConwayGovState ConwayEra)
conwaygovX pp = do
  env <- genFromSpec @ConwayFn @(GovEnv ConwayEra) (govEnvSpec pp)
  genFromSpec @ConwayFn @(ConwayGovState ConwayEra) (conwayGovStateSpec pp env)

lsX :: forall era. EraSpecLedger era ConwayFn => PParams era -> Gen (LedgerState era)
lsX pp = do
  acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
  epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
  genFromSpec @ConwayFn @(LedgerState era) (ledgerStateSpec pp (lit acct) (lit epoch))

esX :: forall era. EraSpecLedger era ConwayFn => PParams era -> Gen (EpochState era)
esX pp = do
  epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
  genFromSpec @ConwayFn @(EpochState era) (epochStateSpec pp (lit epoch))

nesX :: forall era. EraSpecLedger era ConwayFn => PParams era -> Gen (NewEpochState era)
nesX pp = genFromSpec @ConwayFn @(NewEpochState era) (newEpochStateSpec pp)

snapX :: Gen SnapShot
snapX = genFromSpec @ConwayFn @SnapShot snapShotSpec

snapsX :: forall era. EraSpecLedger era ConwayFn => PParams era -> Gen SnapShots
snapsX pp = do
  acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
  epoch <- genFromSpec @ConwayFn @EpochNo epochNoSpec
  ls <- genFromSpec @ConwayFn @(LedgerState era) (ledgerStateSpec pp (lit acct) (lit epoch))
  genFromSpec @ConwayFn @SnapShots (snapShotsSpec (lit (getMarkSnapShot ls)))

instanRewX :: forall era. EraSpecTxOut era ConwayFn => Gen InstantaneousRewards
instanRewX = do
  acct <- genFromSpec @ConwayFn @AccountState accountStateSpec
  genFromSpec @ConwayFn @(InstantaneousRewards)
    (irewardSpec @era @ConwayFn (lit acct))

-- ==============================================================
-- The WellFormed class
-- ==============================================================

class (EraSpecPParams era, HasSpec ConwayFn t) => WellFormed t era where
  -- \| Well formed with PParams as input
  wffWithPP :: PParams era -> Gen t
  wffWithPP _ = wff @t @era

  -- \| Generate a constrained PParams if one is needed
  wff :: Gen t
  wff = do
    pp <- ppX @era
    wffWithPP pp

-- ==============================================================
-- The WellFormed instances
-- ==============================================================

instance EraSpecPParams era => WellFormed (PParams era) era where
  wff = ppX @era
  wffWithPP p = pure p

instance EraSpecPParams era => WellFormed AccountState era where
  wff = acctX
  wffWithPP _ = acctX

instance EraSpecPParams era => WellFormed (PState era) era where
  wff = psX
  wffWithPP _ = psX

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (DState era) era where
  wff = dsX
  wffWithPP _ = dsX

instance EraSpecPParams era => WellFormed (VState era) era where
  wff = vsX
  wffWithPP _ = vsX

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (CertState era) era where
  wff = csX

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (UTxO era) era where
  wff = utxoX

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (UTxOState era) era where
  wffWithPP = utxostateX

instance WellFormed (GovEnv ConwayEra) ConwayEra where
  wffWithPP pp = govenvX pp

instance WellFormed (ConwayGovState ConwayEra) ConwayEra where
  wffWithPP pp = conwaygovX pp

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (ShelleyGovState era) era where
  wffWithPP pp = genFromSpec @ConwayFn @(ShelleyGovState era) (shelleyGovStateSpec pp)

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (LedgerState era) era where
  wffWithPP = lsX

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (EpochState era) era where
  wffWithPP = esX

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed (NewEpochState era) era where
  wffWithPP = nesX

instance EraSpecPParams era => WellFormed SnapShot era where
  wff = snapX

instance (EraSpecPParams era, EraSpecLedger era ConwayFn) => WellFormed SnapShots era where
  wffWithPP = snapsX @era

instance
  (EraSpecPParams era, EraSpecTxOut era ConwayFn) =>
  WellFormed InstantaneousRewards era
  where
  wff = instanRewX @era
