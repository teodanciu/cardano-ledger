{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert (nameTxCert) where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert (ConwayTxCert (..))
import Constrained (lit)
import Data.Map.Strict (Map)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Deleg (nameDelegCert)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.GovCert (nameGovCert)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Pool (namePoolCert)
import Test.Cardano.Ledger.Constrained.Conway

instance
  ( IsConwayUniv fn
  , Inject (ConwayCertExecContext ConwayEra) (Map RewardAccount Coin)
  ) =>
  ExecSpecRule fn "CERT" ConwayEra
  where
  type ExecContext fn "CERT" ConwayEra = ConwayCertExecContext ConwayEra
  environmentSpec _ = certEnvSpec
  stateSpec ctx _ = certStateSpec (lit $ ccecDelegatees ctx)
  signalSpec _ = txCertSpec
  runAgdaRule env st sig = unComputationResult $ Agda.certStep env st sig

  classOf = Just . nameTxCert

nameTxCert :: ConwayTxCert ConwayEra -> String
nameTxCert (ConwayTxCertDeleg x) = nameDelegCert x
nameTxCert (ConwayTxCertPool x) = namePoolCert x
nameTxCert (ConwayTxCertGov x) = nameGovCert x
