{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert () where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert
import Constrained
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base
import Test.Cardano.Ledger.Constrained.Conway

instance
  IsConwayUniv fn =>
  ExecSpecRule fn "CERT" Conway
  where
  type ExecContext fn "CERT" Conway = ConwayCertExecContext Conway
  environmentSpec _ = certEnvSpec
  stateSpec _ _ = certStateSpec
  signalSpec _ env st = txCertSpec env st <> constrained disableCerts
    where
      -- TODO Investigate why! During Adjustment.
      disableCerts :: Term fn (ConwayTxCert Conway) -> Pred fn
      disableCerts cert =
        (caseOn cert)
          ( branch $ \delegCert ->
              (caseOn delegCert)
                (branch $ \_ _ -> True) -- TODO DelegRegCert is disabled! Investigate why!
                (branch $ \_ _ -> True)
                (branch $ \_ _ -> True)
                (branch $ \_ _ _ -> True)
          )
          (branch $ \_ -> True)
          (branch disableDRepRegCerts)
  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.certStep env st sig
