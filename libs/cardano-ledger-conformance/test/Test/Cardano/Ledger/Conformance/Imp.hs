{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Cardano.Ledger.Conformance.Imp (spec) where

import Cardano.Ledger.Alonzo.Tx (AlonzoTx)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (LedgerEnv, UtxoEnv (..), ledgerSlotNoL)
import Control.State.Transition
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (bimapM)
import Data.List.NonEmpty
import Lens.Micro
import Lib qualified as Agda
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway (ConwayLedgerExecContext (..))
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway (ConwayTxBodyTransContext)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Core
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Conway.Imp qualified as ConwayImp (conwaySpec)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common hiding (Args)
import UnliftIO (evaluateDeep)

-- TODO remove this once we get rid of the CPP directives
{- FOURMOLU_DISABLE -}
testImpConformance ::
  forall era.
  ( ConwayEraImp era
  , ExecSpecRule ConwayFn "LEDGER" era
  , ExecContext ConwayFn "LEDGER" era ~ ConwayLedgerExecContext era
  , ExecSignal ConwayFn "LEDGER" era ~ AlonzoTx era
  , ExecState ConwayFn "LEDGER" era ~ LedgerState era
  , SpecTranslate (ExecContext ConwayFn "LEDGER" era) (ExecState ConwayFn "LEDGER" era)
  , SpecTranslate (ExecContext ConwayFn "LEDGER" era) (ExecEnvironment ConwayFn "LEDGER" era)
  , SpecTranslate (ExecContext ConwayFn "LEDGER" era) (TxWits era)
  , HasCallStack
  , SpecRep (TxWits era) ~ Agda.TxWitnesses
  , SpecRep (TxBody era) ~ Agda.TxBody
  , ExecEnvironment ConwayFn "LEDGER" era ~ LedgerEnv era
  , Tx era ~ AlonzoTx era
  , SpecTranslate ConwayTxBodyTransContext (TxBody era)
  , ToExpr (SpecRep (PredicateFailure (EraRule "LEDGER" era)))
  ) =>
  Globals ->
  Either
    (NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
    (State (EraRule "LEDGER" era), [Event (EraRule "LEDGER" era)]) ->
  ExecEnvironment ConwayFn "LEDGER" era ->
  ExecState ConwayFn "LEDGER" era ->
  ExecSignal ConwayFn "LEDGER" era ->
  BaseImpM ()
testImpConformance _globals impRuleResult env state signal = do
  let ctx =
        ConwayLedgerExecContext
          { clecPolicyHash =
              state ^. lsUTxOStateL . utxosGovStateL . constitutionGovStateL . constitutionScriptL
          , clecEnactState = mkEnactState $ state ^. lsUTxOStateL . utxosGovStateL
          , clecUtxoExecContext =
              UtxoExecContext
                { uecTx = signal
                , uecUTxO = state ^. lsUTxOStateL . utxosUtxoL
                , uecUtxoEnv =
                    UtxoEnv
                      { ueSlot = env ^. ledgerSlotNoL
                      , uePParams = state ^. lsUTxOStateL . utxosGovStateL . curPParamsGovStateL
                      , ueCertState = state ^. lsCertStateL
                      }
                }
          }
  -- translate inputs
  (specEnv, specState, specSignal) <-
    (,,)
      <$> expectRight (runSpecTransM ctx $ toSpecRep env)
      <*> expectRight (runSpecTransM ctx $ toSpecRep state)
      <*> expectRight (runSpecTransM ctx $ toSpecRep signal)
  -- get agda response
  agdaResponse <-
    fmap (second fixup) $
      evaluateDeep $
        runAgdaRule @ConwayFn @"LEDGER" @era specEnv specState specSignal
  -- translate imp response
  impResponse <-
    expectRightExpr $
      runSpecTransM ctx $
        bimapM
          (pure . showOpaqueErrorString)
          (toTestRep . inject @_ @(ExecState ConwayFn "LEDGER" era) . fst)
          impRuleResult

#if __GLASGOW_HASKELL__ >= 906
  logString "implEnv"
  logToExpr env
  logString "implState"
  logToExpr state
  logString "implSignal"
  logToExpr signal
  logString "specEnv"
  logToExpr specEnv
  logString "specState"
  logToExpr specState
  logString "specSignal"
  logToExpr specSignal
  logString "Extra info:"
  logDoc $
    extraInfo @ConwayFn @"LEDGER" @era
      _globals
      ctx
      env
      state
      signal
      (first showOpaqueErrorString impRuleResult)
  logDoc $ diffConformance impResponse agdaResponse
#endif
  when (impResponse /= agdaResponse) $
    assertFailure "Conformance failure"
{- FOURMOLU_ENABLE -}

spec :: Spec
spec =
  withImpInit @(LedgerSpec ConwayEra) $
    modifyImpInitProtVer @ConwayEra (natVersion @10) $
      modifyImpInitExpectLedgerRuleConformance testImpConformance $ do
        describe "Basic imp conformance" $ do
          it "Submit constitution" $ do
            _ <- submitConstitution @ConwayEra SNothing
            passNEpochs 2
        xdescribe "Conway Imp conformance" $ ConwayImp.conwaySpec @ConwayEra
