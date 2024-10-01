{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.GovSpec (
  spec,
  relevantDuringBootstrapSpec,
) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (ConwayGovPredFailure (..))
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.Plutus.CostModels (updateCostModels)
import qualified Cardano.Ledger.Shelley.HardForks as HF
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.Val (zero, (<->))
import Data.Default.Class (Default (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Tree
import Lens.Micro
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common hiding (Success)

spec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = do
  relevantDuringBootstrapSpec
  constitutionSpec
  proposalsWithVotingSpec
  votingSpec
  policySpec
  predicateFailuresSpec
  unknownCostModelsSpec

relevantDuringBootstrapSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
relevantDuringBootstrapSpec = do
  withdrawalsSpec
  hardForkSpec
  pparamUpdateSpec
  proposalsSpec
  networkIdSpec
  bootstrapPhaseSpec

unknownCostModelsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
unknownCostModelsSpec =
  describe "Unknown CostModels" $ do
    it "Are accepted" $ do
      costModels <- getsPParams ppCostModelsL
      newCostModels <- arbitrary
      hotCommitteeCs <- registerInitialCommittee
      (drepC, _, _) <- setupSingleDRep 1_000_000
      gai <-
        submitParameterChange SNothing $
          emptyPParamsUpdate
            & ppuCostModelsL .~ SJust newCostModels
      submitYesVote_ (DRepVoter drepC) gai
      submitYesVoteCCs_ hotCommitteeCs gai
      passNEpochs 2
      getLastEnactedParameterChange `shouldReturn` SJust (GovPurposeId gai)
      getsPParams ppCostModelsL `shouldReturn` updateCostModels costModels newCostModels

predicateFailuresSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
predicateFailuresSpec =
  describe "Predicate failures" $ do
    it "ExpirationEpochTooSmall" $ do
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      committeeC <- KeyHashObj <$> freshKeyHash
      rewardAccount <- registerRewardAccount
      anchor <- arbitrary
      let expiration = EpochNo 1
          action =
            UpdateCommittee
              SNothing
              mempty
              (Map.singleton committeeC expiration)
              (0 %! 1)
      passEpoch
      submitFailingProposal
        ( ProposalProcedure
            { pProcReturnAddr = rewardAccount
            , pProcGovAction = action
            , pProcDeposit = pp ^. ppGovActionDepositL
            , pProcAnchor = anchor
            }
        )
        [injectFailure $ ExpirationEpochTooSmall $ Map.singleton committeeC expiration]
    -- TODO: mark as bootstrap relevant
    it "ProposalDepositIncorrect" $ do
      rewardAccount <- registerRewardAccount
      actionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
      anchor <- arbitrary
      submitFailingProposal
        ( ProposalProcedure
            { pProcReturnAddr = rewardAccount
            , pProcGovAction = InfoAction
            , pProcDeposit = actionDeposit <-> Coin 1
            , pProcAnchor = anchor
            }
        )
        [injectFailure $ ProposalDepositIncorrect (actionDeposit <-> Coin 1) actionDeposit]
    it "ConflictingCommitteeUpdate" $ do
      committeeC <- KeyHashObj <$> freshKeyHash
      curEpochNo <- getsNES nesELL
      let action =
            UpdateCommittee
              SNothing
              (Set.singleton committeeC)
              (Map.singleton committeeC (addEpochInterval curEpochNo (EpochInterval 1)))
              (1 %! 1)
      submitFailingGovAction
        action
        [injectFailure $ ConflictingCommitteeUpdate $ Set.singleton committeeC]

hardForkSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
hardForkSpec =
  describe "HardFork" $ do
    describe "Hardfork is the first one (doesn't have a GovPurposeId) " $ do
      it "Hardfork minorFollow" (firstHardForkFollows minorFollow)
      it "Hardfork majorFollow" (firstHardForkFollows majorFollow)
      it "Hardfork cantFollow" firstHardForkCantFollow
    describe "Hardfork is the second one (has a GovPurposeId)" $ do
      it "Hardfork minorFollow" (secondHardForkFollows minorFollow)
      it "Hardfork majorFollow" (secondHardForkFollows majorFollow)
      it "Hardfork cantFollow" secondHardForkCantFollow

pparamUpdateSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
pparamUpdateSpec =
  describe "PParamUpdate" $ do
    describe "PPU needs to be wellformed" $ do
      let testMalformedProposal lbl lenz val = it lbl $ do
            pp <- getsNES $ nesEsL . curPParamsEpochStateL
            rew <- registerRewardAccount
            let ppUpdate =
                  emptyPParamsUpdate
                    & lenz .~ SJust val
                ga = ParameterChange SNothing ppUpdate SNothing
            submitFailingProposal
              ( ProposalProcedure
                  { pProcReturnAddr = rew
                  , pProcGovAction = ga
                  , pProcDeposit = pp ^. ppGovActionDepositL
                  , pProcAnchor = def
                  }
              )
              [injectFailure $ MalformedProposal ga]
      testMalformedProposal
        "ppuMaxBBSizeL cannot be 0"
        ppuMaxBBSizeL
        0
      testMalformedProposal
        "ppuMaxTxSizeL cannot be 0"
        ppuMaxTxSizeL
        0
      testMalformedProposal
        "ppuMaxBHSizeL cannot be 0"
        ppuMaxBHSizeL
        0
      testMalformedProposal
        "ppuMaxValSizeL cannot be 0"
        ppuMaxValSizeL
        0
      testMalformedProposal
        "ppuCollateralPercentageL cannot be 0"
        ppuCollateralPercentageL
        0
      testMalformedProposal
        "ppuCommitteeMaxTermLengthL cannot be 0"
        ppuCommitteeMaxTermLengthL
        $ EpochInterval 0
      testMalformedProposal
        "ppuGovActionLifetimeL cannot be 0"
        ppuGovActionLifetimeL
        $ EpochInterval 0
      testMalformedProposal
        "ppuPoolDepositL cannot be 0"
        ppuPoolDepositL
        zero
      testMalformedProposal
        "ppuGovActionDepositL cannot be 0"
        ppuGovActionDepositL
        zero
      testMalformedProposal
        "ppuDRepDepositL cannot be 0"
        ppuDRepDepositL
        zero
      it "PPU cannot be empty" $ do
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        rew <- registerRewardAccount
        let ga = ParameterChange SNothing emptyPParamsUpdate SNothing
        submitFailingProposal
          ( ProposalProcedure
              { pProcReturnAddr = rew
              , pProcGovAction = ga
              , pProcDeposit = pp ^. ppGovActionDepositL
              , pProcAnchor = def
              }
          )
          [injectFailure $ MalformedProposal ga]

proposalsWithVotingSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
proposalsWithVotingSpec =
  describe "Proposals" $ do
    describe "Consistency" $ do
      it "Subtrees are pruned when competing proposals are enacted" $ do
        (dRep, committeeMember, GovPurposeId committeeGovActionId) <- electBasicCommittee
        a@[ _
            , b@(Node p2 _)
            ] <-
          submitConstitutionForest
            SNothing
            [ Node
                ()
                [ Node
                    ()
                    [ Node () []
                    , Node () []
                    ]
                ]
            , Node
                ()
                [ Node () []
                ]
            ]

        getProposalsForest
          `shouldReturn` [ Node SNothing []
                         , Node SNothing []
                         , Node (SJust committeeGovActionId) []
                         , Node SNothing (fmap SJust <$> a)
                         ]
        passEpoch
        submitYesVote_ (DRepVoter dRep) p2
        submitYesVote_ (CommitteeVoter committeeMember) p2
        passNEpochs 2
        getProposalsForest
          `shouldReturn` [ Node SNothing []
                         , Node SNothing []
                         , Node (SJust committeeGovActionId) []
                         , SJust <$> b
                         ]
      it "Subtrees are pruned when competing proposals are enacted over multiple rounds" $ do
        committeeMembers' <- registerInitialCommittee
        (drepC, _, _) <- setupSingleDRep 1_000_000
        a@[ c
            , Node
                p2
                [ Node p21 []
                  , Node p22 []
                  ]
            , Node p3 []
            ] <-
          submitConstitutionForest
            SNothing
            [ Node
                ()
                [ Node
                    ()
                    [ Node () []
                    , Node () []
                    ]
                ]
            , Node
                ()
                [ Node () []
                , Node () []
                ]
            , Node () []
            ]
        submitYesVote_ (DRepVoter drepC) p2
        submitYesVoteCCs_ committeeMembers' p2
        submitYesVote_ (DRepVoter drepC) p21
        submitYesVoteCCs_ committeeMembers' p21
        submitYesVote_ (DRepVoter drepC) p3
        submitYesVoteCCs_ committeeMembers' p3 -- Two competing proposals break the tie based on proposal order
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node SNothing (fmap SJust <$> a)
        passEpoch
        p4 <- submitConstitutionGovAction SNothing
        p31 <- submitConstitutionGovAction $ SJust p3
        p211 <- submitConstitutionGovAction $ SJust p21
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node
            SNothing
            [ SJust <$> c
            , Node
                (SJust p2)
                [ Node (SJust p21) [Node (SJust p211) []]
                , Node (SJust p22) []
                ]
            , Node (SJust p3) [Node (SJust p31) []]
            , Node (SJust p4) []
            ]
        passEpoch
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node
            (SJust p2)
            [ Node (SJust p21) [Node (SJust p211) []]
            , Node (SJust p22) []
            ]
        [ Node p212 []
          , Node p213 []
          , Node p214 []
          ] <-
          submitConstitutionForest
            (SJust p21)
            [ Node () []
            , Node () []
            , Node () []
            ]
        p2131 <- submitConstitutionGovAction $ SJust p213
        p2141 <- submitConstitutionGovAction $ SJust p214
        submitYesVote_ (DRepVoter drepC) p212
        submitYesVoteCCs_ committeeMembers' p212
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node
            (SJust p2)
            [ Node
                (SJust p21)
                [ Node (SJust p211) []
                , Node (SJust p212) []
                , Node (SJust p213) [Node (SJust p2131) []]
                , Node (SJust p214) [Node (SJust p2141) []]
                ]
            , Node (SJust p22) []
            ]
        passNEpochs 2
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node (SJust p212) []
        props <- getProposals
        proposalsSize props `shouldBe` 0
      it "Votes from subsequent epochs are considered for ratification" $ do
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
        committeeMembers' <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        [Node p1 []] <-
          submitConstitutionForest
            SNothing
            [Node () []]
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node SNothing [Node (SJust p1) []]
        passNEpochs 2
        submitYesVote_ (DRepVoter dRep) p1
        submitYesVoteCCs_ committeeMembers' p1
        passNEpochs 2
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node (SJust p1) []
      it "Subtrees are pruned for both enactment and expiry over multiple rounds" $ do
        committeeMembers' <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
        [ a@( Node
                p1
                [ b@( Node
                        p11
                        [ Node _p111 []
                          , Node _p112 []
                          ]
                      )
                  ]
              )
          , Node
              _p2
              [ Node _p21 []
                , Node _p22 []
                ]
          , Node p3 []
          ] <-
          submitConstitutionForest
            SNothing
            [ Node
                ()
                [ Node
                    ()
                    [ Node () []
                    , Node () []
                    ]
                ]
            , Node
                ()
                [ Node () []
                , Node () []
                ]
            , Node () []
            ]
        passNEpochs 2
        submitYesVote_ (DRepVoter dRep) p1
        submitYesVoteCCs_ committeeMembers' p1
        submitYesVote_ (DRepVoter dRep) p11
        submitYesVoteCCs_ committeeMembers' p11
        submitYesVote_ (DRepVoter dRep) p3
        submitYesVoteCCs_ committeeMembers' p3 -- Two competing proposals break the tie based on proposal order
        passNEpochs 2
        fmap (!! 3) getProposalsForest
          `shouldReturn` SJust
          <$> a
        passEpoch -- ConstitutionPurpose is a delayed action
        fmap (!! 3) getProposalsForest
          `shouldReturn` SJust
          <$> b
        passNEpochs 2
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node (SJust p11) []
        c@[ Node _p113 []
            , Node _p114 []
            ] <-
          submitConstitutionForest
            (SJust p11)
            [ Node () []
            , Node () []
            ]
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node (SJust p11) (fmap SJust <$> c)
        passNEpochs 4
        d@[ Node _p115 []
            , Node p116 []
            ] <-
          submitConstitutionForest
            (SJust p11)
            [ Node () []
            , Node () []
            ]
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node (SJust p11) (fmap SJust <$> (c <> d))
        passNEpochs 2
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node (SJust p11) (fmap SJust <$> d)
        submitYesVote_ (DRepVoter dRep) p116
        submitYesVoteCCs_ committeeMembers' p116
        passNEpochs 3
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node (SJust p116) []
    it "Proposals are stored in the expected order" $ do
      modifyPParams $ ppMaxValSizeL .~ 1_000_000_000
      deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
      ens <- getEnactState
      returnAddr <- registerRewardAccount
      withdrawal <-
        Map.singleton returnAddr . Coin . getPositive
          <$> (arbitrary :: ImpTestM era (Positive Integer))
      let
        mkProp name action = do
          ProposalProcedure
            { pProcReturnAddr = returnAddr
            , pProcGovAction = action
            , pProcDeposit = deposit
            , pProcAnchor = Anchor (fromJust $ textToUrl 16 name) def
            }
        prop0 = mkProp "prop0" InfoAction
        prop1 = mkProp "prop1" $ NoConfidence (ens ^. ensPrevCommitteeL)
        prop2 = mkProp "prop2" InfoAction
        prop3 = mkProp "prop3" $ TreasuryWithdrawals withdrawal SNothing
      submitProposal_ prop0
      submitProposal_ prop1
      let
        checkProps l = do
          props <-
            getsNES $
              nesEsL . epochStateGovStateL @era . cgsProposalsL . pPropsL
          fmap (pProcAnchor . gasProposalProcedure . snd) (OMap.assocList props)
            `shouldBe` fmap pProcAnchor l
      checkProps [prop0, prop1]
      submitProposal_ prop2
      submitProposal_ prop3
      checkProps [prop0, prop1, prop2, prop3]
  where
    submitConstitutionForest = submitGovActionForest submitConstitutionGovAction

proposalsSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
proposalsSpec = do
  describe "Voters" $ do
    it "VotersDoNotExist" $ do
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      let ProtVer major minor = pp ^. ppProtocolVersionL
      gaId <- submitGovAction $ HardForkInitiation SNothing $ ProtVer major (succ minor)
      hotCred <- KeyHashObj <$> freshKeyHash
      submitFailingVote (CommitteeVoter hotCred) gaId $
        [injectFailure $ VotersDoNotExist [CommitteeVoter hotCred]]
      poolId <- freshKeyHash
      submitFailingVote (StakePoolVoter poolId) gaId $
        [injectFailure $ VotersDoNotExist [StakePoolVoter poolId]]
      dRepCred <- KeyHashObj <$> freshKeyHash
      whenPostBootstrap $ do
        submitFailingVote (DRepVoter dRepCred) gaId [injectFailure $ VotersDoNotExist [DRepVoter dRepCred]]
    it "DRep votes are removed" $ do
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      gaId <- submitGovAction InfoAction
      dRepCred <- KeyHashObj <$> registerDRep
      submitVote_ VoteNo (DRepVoter dRepCred) gaId
      gas <- getGovActionState gaId
      gasDRepVotes gas `shouldBe` [(dRepCred, VoteNo)]
      let deposit = pp ^. ppDRepDepositL
      submitTx_ $ mkBasicTx (mkBasicTxBody & certsTxBodyL .~ [UnRegDRepTxCert dRepCred deposit])
      gasAfterRemoval <- getGovActionState gaId
      gasDRepVotes gasAfterRemoval `shouldBe` []
  describe "Proposals" $ do
    it "Predicate failure when proposal deposit has nonexistent return address" $ do
      protVer <- getProtVer
      registeredRewardAccount <- registerRewardAccount
      unregisteredRewardAccount <- freshKeyHash >>= getRewardAccountFor . KeyHashObj
      deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
      anchor <- arbitrary
      let mkProposal rewardAccount =
            ProposalProcedure
              { pProcDeposit = deposit
              , pProcReturnAddr = rewardAccount
              , pProcGovAction = InfoAction
              , pProcAnchor = anchor
              }
      if HF.bootstrapPhase protVer
        then do
          submitProposal_ $ mkProposal registeredRewardAccount
          submitProposal_ $ mkProposal unregisteredRewardAccount
        else do
          submitProposal_ $ mkProposal registeredRewardAccount
          submitFailingProposal
            (mkProposal unregisteredRewardAccount)
            [ injectFailure $ ProposalReturnAccountDoesNotExist unregisteredRewardAccount
            ]
    describe "Consistency" $ do
      it "Proposals submitted without proper parent fail" $ do
        let mkCorruptGovActionId :: GovActionId c -> GovActionId c
            mkCorruptGovActionId (GovActionId txi (GovActionIx gaix)) =
              GovActionId txi $ GovActionIx $ gaix + 999
        Node p1 [Node _p11 []] <-
          submitParameterChangeTree
            SNothing
            $ Node
              ()
              [ Node () []
              ]
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        rewardAccount <- registerRewardAccount
        let parameterChangeAction =
              ParameterChange
                (SJust $ GovPurposeId $ mkCorruptGovActionId p1)
                (def & ppuMinFeeAL .~ SJust (Coin 3000))
                SNothing
            parameterChangeProposal =
              ProposalProcedure
                { pProcDeposit = pp ^. ppGovActionDepositL
                , pProcReturnAddr = rewardAccount
                , pProcGovAction = parameterChangeAction
                , pProcAnchor = def
                }
        submitFailingProposal
          parameterChangeProposal
          [ injectFailure $ InvalidPrevGovActionId parameterChangeProposal
          ]
      it "Subtrees are pruned when proposals expire" $ do
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
        p1 <- submitParameterChange SNothing (def & ppuMinFeeAL .~ SJust (Coin 3000))
        passNEpochs 3
        a <-
          submitParameterChangeTree
            (SJust p1)
            $ Node
              ()
              [ Node () []
              , Node () []
              ]
        b <-
          submitParameterChangeTree
            SNothing
            $ Node
              ()
              [ Node () []
              ]
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p1) [SJust <$> a]
                            , SJust <$> b
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passNEpochs 3
        getProposalsForest
          `shouldReturn` [ Node SNothing [SJust <$> b]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
      it "Subtrees are pruned when proposals expire over multiple rounds" $ do
        let ppupdate = def & ppuMinFeeAL .~ SJust (Coin 3000)
        let submitInitialProposal = submitParameterChange SNothing ppupdate
        let submitChildProposal parent = submitParameterChange (SJust parent) ppupdate
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
        p1 <- submitInitialProposal
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p1) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]

        passEpoch
        p2 <- submitInitialProposal
        p11 <- submitChildProposal p1
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p1) [Node (SJust p11) []]
                            , Node (SJust p2) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]

        passEpoch
        p3 <- submitInitialProposal
        p21 <- submitChildProposal p2
        a <-
          submitParameterChangeForest
            (SJust p11)
            [ Node () []
            , Node () []
            ]
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node
                                (SJust p1)
                                [ Node
                                    (SJust p11)
                                    (fmap SJust <$> a)
                                ]
                            , Node (SJust p2) [Node (SJust p21) []]
                            , Node (SJust p3) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]

        passEpoch
        p4 <- submitInitialProposal
        p31 <- submitChildProposal p3
        p211 <- submitChildProposal p21
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node
                                (SJust p1)
                                [ Node
                                    (SJust p11)
                                    (fmap SJust <$> a)
                                ]
                            , Node (SJust p2) [Node (SJust p21) [Node (SJust p211) []]]
                            , Node (SJust p3) [Node (SJust p31) []]
                            , Node (SJust p4) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passNEpochs 3
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p2) [Node (SJust p21) [Node (SJust p211) []]]
                            , Node (SJust p3) [Node (SJust p31) []]
                            , Node (SJust p4) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        p5 <- submitInitialProposal
        p41 <- submitChildProposal p4
        p311 <- submitChildProposal p31
        p212 <- submitChildProposal p21
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node
                                (SJust p2)
                                [ Node
                                    (SJust p21)
                                    [ Node (SJust p211) []
                                    , Node (SJust p212) []
                                    ]
                                ]
                            , Node (SJust p3) [Node (SJust p31) [Node (SJust p311) []]]
                            , Node (SJust p4) [Node (SJust p41) []]
                            , Node (SJust p5) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passEpoch
        p6 <- submitInitialProposal
        p51 <- submitChildProposal p5
        p411 <- submitChildProposal p41
        p312 <- submitChildProposal p31
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node
                                (SJust p3)
                                [ Node
                                    (SJust p31)
                                    [ Node (SJust p311) []
                                    , Node (SJust p312) []
                                    ]
                                ]
                            , Node (SJust p4) [Node (SJust p41) [Node (SJust p411) []]]
                            , Node (SJust p5) [Node (SJust p51) []]
                            , Node (SJust p6) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passEpoch
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p4) [Node (SJust p41) [Node (SJust p411) []]]
                            , Node (SJust p5) [Node (SJust p51) []]
                            , Node (SJust p6) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passEpoch
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p5) [Node (SJust p51) []]
                            , Node (SJust p6) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passNEpochs 3
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p6) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passEpoch
        getProposalsForest
          `shouldReturn` [ Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
  where
    submitParameterChangeForest = submitGovActionForest $ submitGovAction . paramAction
    submitParameterChangeTree = submitGovActionTree $ submitGovAction . paramAction
    paramAction p =
      ParameterChange (GovPurposeId <$> p) (def & ppuMinFeeAL .~ SJust (Coin 10)) SNothing

votingSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
votingSpec =
  describe "Voting" $ do
    describe "fails for" $ do
      it "expired gov-actions" $ do
        -- Voting after the 3rd epoch should fail
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
        (drep, _, _) <- setupSingleDRep 1_000_000
        (govActionId, _) <- submitConstitution SNothing
        passNEpochs 3
        submitFailingVote
          (DRepVoter drep)
          govActionId
          [ injectFailure $ VotingOnExpiredGovAction [(DRepVoter drep, govActionId)]
          ]
      it "non-existent gov-actions" $ do
        (drep, _, _) <- setupSingleDRep 1_000_000
        (govActionId, _) <- submitConstitution SNothing
        let dummyGaid = govActionId {gaidGovActionIx = GovActionIx 99} -- non-existent `GovActionId`
        submitFailingVote
          (DRepVoter drep)
          dummyGaid
          [injectFailure $ GovActionsDoNotExist $ pure dummyGaid]
      it "committee member can not vote on UpdateCommittee action" $ do
        (ccHot :| _) <- registerInitialCommittee
        newMembers <- listOf $ do
          newCommitteeMember <- KeyHashObj <$> freshKeyHash
          Positive lifetime <- arbitrary
          pure (newCommitteeMember, EpochInterval lifetime)
        threshold <- arbitrary
        committeeUpdateId <- submitUpdateCommittee Nothing mempty newMembers threshold
        let voter = CommitteeVoter ccHot
        submitFailingVote
          voter
          committeeUpdateId
          [ injectFailure $ DisallowedVoters [(voter, committeeUpdateId)]
          ]
      it "committee member can not vote on NoConfidence action" $ do
        hotCred :| _ <- registerInitialCommittee
        gaid <- submitGovAction $ NoConfidence SNothing
        let voter = CommitteeVoter hotCred
        trySubmitVote VoteNo voter gaid
          `shouldReturn` Left
            [ injectFailure $ DisallowedVoters [(voter, gaid)]
            ]
      it "committee member mixed with other voters can not vote on UpdateCommittee action" $ do
        ccVoteOnConstitutionFailsWithMultipleVotes
      it "CC cannot ratify if below threshold" $ do
        modifyPParams $ \pp ->
          pp
            & ppGovActionLifetimeL .~ EpochInterval 3
            & ppCommitteeMinSizeL .~ 2
        (dRepCred, _, _) <- setupSingleDRep 1_000_000
        (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
        ccColdCred0 <- KeyHashObj <$> freshKeyHash
        ccColdCred1 <- KeyHashObj <$> freshKeyHash
        electionGovAction <-
          submitUpdateCommittee
            Nothing
            mempty
            [ (ccColdCred0, EpochInterval 10)
            , (ccColdCred1, EpochInterval 10)
            ]
            (3 %! 5)
        submitYesVote_ (DRepVoter dRepCred) electionGovAction
        submitYesVote_ (StakePoolVoter spoC) electionGovAction
        logAcceptedRatio electionGovAction
        passNEpochs 3
        expectNoCurrentProposals
        ccHotKey0 <- registerCommitteeHotKey ccColdCred0
        ccHotKey1 <- registerCommitteeHotKey ccColdCred1
        anchor <- arbitrary
        constitutionChangeId <-
          submitGovAction $
            NewConstitution
              SNothing
              Constitution
                { constitutionScript = SNothing
                , constitutionAnchor = anchor
                }
        submitYesVote_ (DRepVoter dRepCred) constitutionChangeId
        submitYesVote_ (CommitteeVoter ccHotKey0) constitutionChangeId
        _ <- resignCommitteeColdKey ccColdCred0 SNothing
        submitYesVote_ (CommitteeVoter ccHotKey1) constitutionChangeId
        passEpoch
        logAcceptedRatio constitutionChangeId
        logToExpr =<< lookupGovActionState constitutionChangeId
        passNEpochs 4
        conAnchor <-
          getsNES $
            nesEsL
              . esLStateL
              . lsUTxOStateL
              . utxosGovStateL
              . cgsConstitutionL
              . constitutionAnchorL
        expectNoCurrentProposals
        conAnchor `shouldNotBe` anchor

constitutionSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
constitutionSpec =
  describe "Constitution proposals" $ do
    describe "accepted for" $ do
      it "empty PrevGovId before the first constitution is enacted" $ do
        --  Initial proposal does not need a GovPurposeId but after it is enacted, the
        --  following ones are not
        _ <- submitConstitution SNothing
        -- Until the first proposal is enacted all proposals with empty GovPurposeIds are valid
        void $ submitConstitution SNothing
      it "valid GovPurposeId" $ do
        committeeMembers' <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        constitution <- arbitrary
        gaidConstitutionProp <- enactConstitution SNothing constitution dRep committeeMembers'
        constitution1 <- arbitrary
        void $
          enactConstitution
            (SJust $ GovPurposeId gaidConstitutionProp)
            constitution1
            dRep
            committeeMembers'

    describe "rejected for" $ do
      it "empty PrevGovId after the first constitution was enacted" $ do
        committeeMembers' <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        (govActionId, _constitution) <- submitConstitution SNothing
        submitYesVote_ (DRepVoter dRep) govActionId
        submitYesVoteCCs_ committeeMembers' govActionId
        passNEpochs 2
        constitution <- arbitrary
        let invalidNewConstitutionGovAction =
              NewConstitution
                SNothing
                constitution
        invalidNewConstitutionProposal <- proposalWithRewardAccount invalidNewConstitutionGovAction
        submitFailingProposal
          invalidNewConstitutionProposal
          [ injectFailure $ InvalidPrevGovActionId invalidNewConstitutionProposal
          ]
      it "invalid index in GovPurposeId" $ do
        (govActionId, _constitution) <- submitConstitution SNothing
        passNEpochs 2
        constitution <- arbitrary
        let invalidPrevGovActionId =
              -- Expected Ix = 0
              GovPurposeId (govActionId {gaidGovActionIx = GovActionIx 1})
            invalidNewConstitutionGovAction =
              NewConstitution
                (SJust invalidPrevGovActionId)
                constitution
        invalidNewConstitutionProposal <- proposalWithRewardAccount invalidNewConstitutionGovAction
        submitFailingProposal
          invalidNewConstitutionProposal
          [ injectFailure $ InvalidPrevGovActionId invalidNewConstitutionProposal
          ]
      it "valid GovPurposeId but invalid purpose" $ do
        (govActionId, _constitution) <- submitConstitution SNothing
        passNEpochs 2
        let invalidNoConfidenceAction =
              NoConfidence $ SJust $ GovPurposeId govActionId
        invalidNoConfidenceProposal <- proposalWithRewardAccount invalidNoConfidenceAction

        submitFailingProposal
          invalidNoConfidenceProposal
          [ injectFailure $ InvalidPrevGovActionId invalidNoConfidenceProposal
          ]
    it "submitted successfully with valid GovPurposeId" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 1

      curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
      initialPulser <- getsNES $ newEpochStateGovStateL . drepPulsingStateGovStateL
      initialEnactState <- getEnactState

      (govActionId, _) <- submitConstitution SNothing
      curConstitution' <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
      impAnn "Constitution has not been enacted yet" $
        curConstitution' `shouldBe` curConstitution

      govState <- getsNES newEpochStateGovStateL
      let expectedProposals = govState ^. cgsProposalsL
          expectedPulser = govState ^. cgsDRepPulsingStateL
      expectedEnactState <- getEnactState

      impAnn "EnactState reflects the submitted governance action" $ do
        expectedEnactState `shouldBe` initialEnactState

      impAnn "Proposals contain the submitted proposal" $
        expectedProposals `shouldSatisfy` \props -> govActionId `elem` proposalsIds props

      impAnn "Pulser has not changed" $
        expectedPulser `shouldBe` initialPulser

      passNEpochs 2
      impAnn "Proposal gets removed after expiry" $ do
        govStateFinal <- getsNES newEpochStateGovStateL
        let ratifyState = extractDRepPulsingState (govStateFinal ^. cgsDRepPulsingStateL)
        rsExpired ratifyState `shouldBe` Set.singleton govActionId

policySpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
policySpec =
  describe "Policy" $ do
    it "policy is respected by proposals" $ do
      committeeMembers' <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      keyHash <- freshKeyHash
      scriptHash <- impAddNativeScript $ RequireAllOf (SSeq.singleton (RequireSignature keyHash))
      anchor <- arbitrary
      _ <-
        enactConstitution
          SNothing
          (Constitution anchor (SJust scriptHash))
          dRep
          committeeMembers'
      wrongScriptHash <-
        impAddNativeScript $
          RequireMOf 1 $
            SSeq.fromList [RequireAnyOf mempty, RequireAllOf mempty]
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      impAnn "ParameterChange with correct policy succeeds" $ do
        let
          pparamsUpdate =
            def
              & ppuCommitteeMinSizeL .~ SJust 1
        rewardAccount <- registerRewardAccount
        submitProposal_
          ProposalProcedure
            { pProcReturnAddr = rewardAccount
            , pProcGovAction = ParameterChange SNothing pparamsUpdate (SJust scriptHash)
            , pProcDeposit = pp ^. ppGovActionDepositL
            , pProcAnchor = def
            }

      impAnn "TreasuryWithdrawals with correct policy succeeds" $ do
        rewardAccount <- registerRewardAccount
        let
          withdrawals =
            Map.fromList
              [ (rewardAccount, Coin 1000)
              ]
        submitProposal_
          ProposalProcedure
            { pProcReturnAddr = rewardAccount
            , pProcGovAction = TreasuryWithdrawals withdrawals (SJust scriptHash)
            , pProcDeposit = pp ^. ppGovActionDepositL
            , pProcAnchor = def
            }

      impAnn "ParameterChange with invalid policy fails" $ do
        rewardAccount <- registerRewardAccount
        let
          pparamsUpdate =
            def
              & ppuCommitteeMinSizeL .~ SJust 2
        res <-
          trySubmitProposal
            ProposalProcedure
              { pProcReturnAddr = rewardAccount
              , pProcGovAction = ParameterChange SNothing pparamsUpdate (SJust wrongScriptHash)
              , pProcDeposit = pp ^. ppGovActionDepositL
              , pProcAnchor = def
              }
        res
          `shouldBeLeft` [ injectFailure $
                            InvalidPolicyHash (SJust wrongScriptHash) (SJust scriptHash)
                         ]

      impAnn "TreasuryWithdrawals with invalid policy fails" $ do
        rewardAccount <- registerRewardAccount
        let
          withdrawals =
            Map.fromList
              [ (rewardAccount, Coin 1000)
              ]
        res <-
          trySubmitProposal
            ProposalProcedure
              { pProcReturnAddr = rewardAccount
              , pProcGovAction = TreasuryWithdrawals withdrawals (SJust wrongScriptHash)
              , pProcDeposit = pp ^. ppGovActionDepositL
              , pProcAnchor = def
              }
        res
          `shouldBeLeft` [ injectFailure $
                            InvalidPolicyHash (SJust wrongScriptHash) (SJust scriptHash)
                         ]

networkIdSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
networkIdSpec =
  describe "Network ID" $ do
    it "Fails with invalid network ID in proposal return address" $ do
      rewardCredential <- KeyHashObj <$> freshKeyHash
      let badRewardAccount =
            RewardAccount
              { raNetwork = Mainnet -- Our network is Testnet
              , raCredential = rewardCredential
              }
      propDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
      pv <- getProtVer
      let proposal =
            ProposalProcedure
              { pProcReturnAddr = badRewardAccount
              , pProcGovAction = InfoAction
              , pProcDeposit = propDeposit
              , pProcAnchor = def
              }
      if HF.bootstrapPhase pv
        then
          submitFailingProposal
            proposal
            [ injectFailure $
                ProposalProcedureNetworkIdMismatch
                  badRewardAccount
                  Testnet
            ]
        else
          submitFailingProposal
            proposal
            [ injectFailure $
                ProposalReturnAccountDoesNotExist
                  badRewardAccount
            , injectFailure $
                ProposalProcedureNetworkIdMismatch
                  badRewardAccount
                  Testnet
            ]

withdrawalsSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
withdrawalsSpec =
  describe "Withdrawals" $ do
    it "Fails predicate when treasury withdrawal has nonexistent return address" $ do
      policy <- getGovPolicy
      unregisteredRewardAccount <- freshKeyHash >>= getRewardAccountFor . KeyHashObj
      registeredRewardAccount <- registerRewardAccount
      let genPositiveCoin = Coin . getPositive <$> arbitrary
          withdrawalAccountDoesNotExist = TreasuryWithdrawalReturnAccountsDoNotExist [unregisteredRewardAccount]
      withdrawals <-
        sequence
          [ (unregisteredRewardAccount,) <$> genPositiveCoin
          , (registeredRewardAccount,) <$> genPositiveCoin
          ]
      expectPredFailures [withdrawalAccountDoesNotExist] [] $
        TreasuryWithdrawals (Map.fromList withdrawals) policy
    it "Fails with invalid network ID in withdrawal addresses" $ do
      rewardCredential <- KeyHashObj <$> freshKeyHash
      let badRewardAccount =
            RewardAccount
              { raNetwork = Mainnet -- Our network is Testnet
              , raCredential = rewardCredential
              }
          wdrls = TreasuryWithdrawals (Map.singleton badRewardAccount $ Coin 100_000_000) SNothing
          idMismatch = TreasuryWithdrawalsNetworkIdMismatch (Set.singleton badRewardAccount) Testnet
          returnAddress = TreasuryWithdrawalReturnAccountsDoNotExist [badRewardAccount]
      expectPredFailures [returnAddress, idMismatch] [idMismatch] wdrls

    it "Fails for empty withdrawals" $ do
      rwdAccount1 <- registerRewardAccount
      rwdAccount2 <- registerRewardAccount
      let withdrawals = Map.fromList [(rwdAccount1, zero), (rwdAccount2, zero)]
      let wdrls = TreasuryWithdrawals Map.empty SNothing
       in expectPredFailures [ZeroTreasuryWithdrawals wdrls] [] wdrls

      let wdrls = TreasuryWithdrawals [(rwdAccount1, zero)] SNothing
       in expectPredFailures [ZeroTreasuryWithdrawals wdrls] [] wdrls

      let wdrls = TreasuryWithdrawals withdrawals SNothing
       in expectPredFailures [ZeroTreasuryWithdrawals wdrls] [] wdrls

      rwdAccountRegistered <- registerRewardAccount
      let wdrls = TreasuryWithdrawals [(rwdAccountRegistered, zero)] SNothing
       in expectPredFailures [ZeroTreasuryWithdrawals wdrls] [] wdrls

      curProtVer <- getProtVer
      let wdrls = Map.insert rwdAccount2 (Coin 100_000) withdrawals
          ga = TreasuryWithdrawals wdrls SNothing
       in if HF.bootstrapPhase curProtVer
            then do
              expectPredFailures [] [] ga
            else
              submitGovAction_ ga
  where
    expectPredFailures ::
      [ConwayGovPredFailure era] -> [ConwayGovPredFailure era] -> GovAction era -> ImpTestM era ()
    expectPredFailures predFailures bootstrapPredFailures wdrl = do
      curProtVer <- getProtVer
      propP <- proposalWithRewardAccount wdrl
      submitFailingProposal
        propP
        ( injectFailure
            <$> ( if HF.bootstrapPhase curProtVer
                    then DisallowedProposalDuringBootstrap propP NE.:| bootstrapPredFailures
                    else NE.fromList predFailures
                )
        )

proposalWithRewardAccount ::
  forall era.
  ConwayEraImp era =>
  GovAction era ->
  ImpTestM era (ProposalProcedure era)
proposalWithRewardAccount action = do
  rewardAccount <- registerRewardAccount
  govActionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
  pure
    ProposalProcedure
      { pProcDeposit = govActionDeposit
      , pProcReturnAddr = rewardAccount
      , pProcGovAction = action
      , pProcAnchor = def
      }

-- =========================================================
-- Proposing a HardFork should always use a new ProtVer that
-- can follow the one installed in the previous HardFork action.

-- | Tests the first hardfork in the Conway era where the PrevGovActionID is SNothing
firstHardForkFollows ::
  forall era.
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  (ProtVer -> ProtVer) ->
  ImpTestM era ()
firstHardForkFollows computeNewFromOld = do
  protVer <- getProtVer
  submitGovAction_ $ HardForkInitiation SNothing (computeNewFromOld protVer)

-- | Negative (deliberatey failing) first hardfork in the Conway era where the PrevGovActionID is SNothing
firstHardForkCantFollow ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  ImpTestM era ()
firstHardForkCantFollow = do
  rewardAccount <- registerRewardAccount
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let protver0 = pp ^. ppProtocolVersionL
      protver1 = minorFollow protver0
      protver2 = cantFollow protver1
  submitFailingProposal
    ( ProposalProcedure
        { pProcDeposit = pp ^. ppGovActionDepositL
        , pProcReturnAddr = rewardAccount
        , pProcGovAction = HardForkInitiation SNothing protver2
        , pProcAnchor = def
        }
    )
    [injectFailure $ ProposalCantFollow SNothing protver2 protver0]

-- | Tests a second hardfork in the Conway era where the PrevGovActionID is SJust
secondHardForkFollows ::
  forall era.
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  (ProtVer -> ProtVer) ->
  ImpTestM era ()
secondHardForkFollows computeNewFromOld = do
  protver0 <- getProtVer
  let protver1 = minorFollow protver0
      protver2 = computeNewFromOld protver1
  gaid1 <- submitGovAction $ HardForkInitiation SNothing protver1
  submitGovAction_ $ HardForkInitiation (SJust (GovPurposeId gaid1)) protver2

-- | Negative (deliberatey failing) first hardfork in the Conway era where the PrevGovActionID is SJust
secondHardForkCantFollow ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  ImpTestM era ()
secondHardForkCantFollow = do
  rewardAccount <- registerRewardAccount
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let protver0 = pp ^. ppProtocolVersionL
      protver1 = minorFollow protver0
      protver2 = cantFollow protver1
  gaid1 <-
    submitProposal $
      ProposalProcedure
        { pProcDeposit = pp ^. ppGovActionDepositL
        , pProcReturnAddr = rewardAccount
        , pProcGovAction = HardForkInitiation SNothing protver1
        , pProcAnchor = def
        }
  submitFailingProposal
    ( ProposalProcedure
        { pProcDeposit = pp ^. ppGovActionDepositL
        , pProcReturnAddr = rewardAccount
        , pProcGovAction = HardForkInitiation (SJust (GovPurposeId gaid1)) protver2
        , pProcAnchor = def
        }
    )
    [injectFailure $ ProposalCantFollow (SJust (GovPurposeId gaid1)) protver2 protver1]

ccVoteOnConstitutionFailsWithMultipleVotes ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  ImpTestM era ()
ccVoteOnConstitutionFailsWithMultipleVotes = do
  (ccCred :| _) <- registerInitialCommittee
  (drepCred, _, _) <- setupSingleDRep 1_000_000
  drepCred2 <- KeyHashObj <$> registerDRep
  newCommitteeMember <- KeyHashObj <$> freshKeyHash
  committeeProposal <-
    submitUpdateCommittee Nothing mempty [(newCommitteeMember, EpochInterval 10)] (1 %! 2)
  let
    voteTx =
      mkBasicTx $
        mkBasicTxBody
          & votingProceduresTxBodyL
            .~ VotingProcedures
              ( Map.fromList
                  [
                    ( DRepVoter drepCred2
                    , Map.singleton committeeProposal $ VotingProcedure VoteYes SNothing
                    )
                  ,
                    ( CommitteeVoter ccCred
                    , Map.singleton committeeProposal $ VotingProcedure VoteNo SNothing
                    )
                  ,
                    ( DRepVoter drepCred
                    , Map.singleton committeeProposal $ VotingProcedure VoteYes SNothing
                    )
                  ]
              )
  impAnn "Try to vote as a committee member" $
    submitFailingTx
      voteTx
      [ injectFailure $
          DisallowedVoters [(CommitteeVoter ccCred, committeeProposal)]
      ]

bootstrapPhaseSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
bootstrapPhaseSpec =
  describe "Proposing and voting during bootstrap phase" $ do
    it "Parameter change" $ do
      gid <- submitParameterChange SNothing (def & ppuMinFeeAL .~ SJust (Coin 3000))
      (committee :| _) <- registerInitialCommittee
      (drep, _, _) <- setupSingleDRep 1_000_000
      (spo, _, _) <- setupPoolWithStake $ Coin 42_000_000
      checkVotingFailure (DRepVoter drep) gid
      submitYesVote_ (StakePoolVoter spo) gid
      submitYesVote_ (CommitteeVoter committee) gid
    it "Hardfork initiation" $ do
      curProtVer <- getProtVer
      nextMajorVersion <- succVersion $ pvMajor curProtVer
      gid <-
        submitGovAction $
          HardForkInitiation SNothing (curProtVer {pvMajor = nextMajorVersion})
      (committee :| _) <- registerInitialCommittee
      (drep, _, _) <- setupSingleDRep 1_000_000
      (spo, _, _) <- setupPoolWithStake $ Coin 42_000_000
      checkVotingFailure (DRepVoter drep) gid
      submitYesVote_ (StakePoolVoter spo) gid
      submitYesVote_ (CommitteeVoter committee) gid
    it "Info action" $ do
      gid <- submitGovAction InfoAction
      (committee :| _) <- registerInitialCommittee
      (drep, _, _) <- setupSingleDRep 1_000_000
      (spo, _, _) <- setupPoolWithStake $ Coin 42_000_000
      submitYesVote_ (DRepVoter drep) gid
      submitYesVote_ (StakePoolVoter spo) gid
      submitYesVote_ (CommitteeVoter committee) gid
    it "Treasury withdrawal" $ do
      rewardAccount <- registerRewardAccount
      govActionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
      let action = TreasuryWithdrawals [(rewardAccount, Coin 1000)] SNothing
      let proposal =
            ProposalProcedure
              { pProcDeposit = govActionDeposit
              , pProcReturnAddr = rewardAccount
              , pProcGovAction = action
              , pProcAnchor = def
              }
      checkProposalFailure proposal
    it "NoConfidence" $ do
      proposal <- proposalWithRewardAccount $ NoConfidence SNothing
      checkProposalFailure proposal
    it "UpdateCommittee" $ do
      cCred <- KeyHashObj <$> freshKeyHash
      curEpochNo <- getsNES nesELL
      let newMembers = [(cCred, addEpochInterval curEpochNo (EpochInterval 30))]
      proposal <- proposalWithRewardAccount $ UpdateCommittee SNothing mempty newMembers (1 %! 1)
      checkProposalFailure proposal
    it "NewConstitution" $ do
      constitution <- arbitrary
      proposal <- proposalWithRewardAccount $ NewConstitution SNothing constitution
      checkProposalFailure proposal
  where
    checkProposalFailure proposal = do
      curProtVer <- getProtVer
      when (HF.bootstrapPhase curProtVer) $
        submitFailingProposal proposal [injectFailure $ DisallowedProposalDuringBootstrap proposal]
    checkVotingFailure voter gid = do
      curProtVer <- getProtVer
      when (HF.bootstrapPhase curProtVer) $
        submitFailingVote voter gid [injectFailure $ DisallowedVotesDuringBootstrap [(voter, gid)]]
