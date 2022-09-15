module Test.Cardano.Api.Fees (tests) where

import           Data.Either (isLeft)
import           Data.Maybe (fromJust)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           Gen.Cardano.Api.Typed (genValueDefault)
import           Hedgehog (Property, assert, forAll, property)
import           Hedgehog.Gen (list)
import           Hedgehog.Range (linear)
import           Prelude
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testPropertyNamed)

import           Cardano.Api
import           Cardano.Api.Shelley (shelleyGenesisDefaults)
import           Ouroboros.Consensus.HardFork.History (mkInterpreter, summarize)
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))

runAutoBalance :: [Value] -> [Value] -> Either TxBodyErrorAutoBalance (BalancedTxBody AlonzoEra)
runAutoBalance _inValues _outValues = makeTransactionBodyAutoBalance
  AlonzoEraInCardanoMode
  (SystemStart $ fromJust $ iso8601ParseM "1970-01-01Z00:00:00")
  (EraHistory CardanoMode (mkInterpreter (summarize _ _ _)))
  pparams
  _
  _
  TxBodyContent
    { txIns = _
    , txInsCollateral = _
    , txInsReference = _
    , txOuts = _
    , txTotalCollateral = _
    , txReturnCollateral = _
    , txFee = _
    , txValidityRange = _
    , txMetadata = _
    , txAuxScripts = _
    , txExtraKeyWits = _
    , txProtocolParams = BuildTxWith (Just pparams)
    , txWithdrawals = _
    , txCertificates = _
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = _
    , txScriptValidity = _
    }
  (AddressInEra
    (ShelleyAddressInEra ShelleyBasedEraAlonzo)
    (makeShelleyAddress (Testnet (NetworkMagic 42))
      (PaymentCredentialByKey _)
     NoStakeAddress))
  Nothing
  where
    pparams = _ (sgProtocolParams shelleyGenesisDefaults)

prop_noFeeFailure :: Property
prop_noFeeFailure = property $ do
  v <- forAll $ list (linear 0 10) genValueDefault
  assert $ isLeft $ runAutoBalance v v

tests :: TestTree
tests =
  testGroup "Cardano.Api.Fees"
    [ testPropertyNamed "fail on no fee" "fail on no fee" prop_noFeeFailure]
