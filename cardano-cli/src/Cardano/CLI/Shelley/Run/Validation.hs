{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Validation
  ( -- * Metadata
    MetadataError(..)
  , readFileTxMetadata
  , readTxMetadata

    -- * Script
  , ScriptWitnessError(..)
  , readScriptDataOrFile
  , readScriptWitness
  , readScriptWitnessFiles
  , readScriptWitnessFilesThruple

  -- * Script data (datums and redeemers)
  , ScriptDataError(..)
  , readScriptDatumOrFile
  , readScriptRedeemerOrFile
  , renderScriptDataError

  , renderEra
  ) where

import           Prelude

import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, handleLeftT,
                   hoistEither, hoistMaybe, left, newExceptT)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Type.Equality (TestEquality (..))
import           Data.Word
import qualified System.IO as IO


import           Cardano.Api
import           Cardano.Api.Byron hiding (SomeByronSigningKey (..))
import           Cardano.Api.Shelley

--TODO: do this nicely via the API too:
import qualified Cardano.Binary as CBOR
import           Data.Text (Text)
--TODO: following import needed for orphan Eq Script instance
import           Cardano.Ledger.Shelley.Scripts ()

import           Cardano.CLI.Run.Friendly (friendlyTxBS, friendlyTxBodyBS)
import           Cardano.CLI.Shelley.Key (InputDecodeError, readSigningKeyFileAnyOf)
import           Cardano.CLI.Shelley.Output
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Shelley.Run.Genesis (ShelleyGenesisCmdError (..),
                   readShelleyGenesisWithDefault)
import           Cardano.CLI.Shelley.Script
import           Cardano.CLI.Types

import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

-- Metadata

data MetadataError
  = MetadataErrorFile (FileError ())
  | MetadataErrorJsonParseError !FilePath !String
  | MetadataErrorConversionError !FilePath !TxMetadataJsonError
  | MetadataErrorValidationError !FilePath ![(Word64, TxMetadataRangeError)]
  | MetadataErrorDecodeError !FilePath !CBOR.DecoderError
  | MetadataErrorNotAvailableInEra AnyCardanoEra

renderMetadataError :: MetadataError -> Text
renderMetadataError (MetadataErrorFile fileErr) =
  Text.pack $ displayError fileErr
renderMetadataError (MetadataErrorJsonParseError fp jsonErr) =
  Text.pack $ "Invalid JSON format in file: " <> show fp <>
              "\nJSON parse error: " <> jsonErr
renderMetadataError (MetadataErrorConversionError fp metadataErr) =
  Text.pack $ "Error reading metadata at: " <> show fp <>
              "\n" <> displayError metadataErr
renderMetadataError (MetadataErrorValidationError fp errs) =
  Text.pack $ "Error validating transaction metadata at: " <> fp <> "\n" <>
      List.intercalate "\n"
        [ "key " <> show k <> ":" <> displayError valErr
        | (k, valErr) <- errs ]
renderMetadataError (MetadataErrorDecodeError fp metadataErr) =
  Text.pack $ "Error decoding CBOR metadata at: " <> show fp <>
              " Error: " <> show metadataErr
renderMetadataError (MetadataErrorNotAvailableInEra (AnyCardanoEra era')) =
  error ""

readTxMetadata :: CardanoEra era
               -> TxMetadataJsonSchema
               -> [MetadataFile]
               -> IO (Either MetadataError (TxMetadataInEra era))
readTxMetadata _ _ [] = return $ Right TxMetadataNone
readTxMetadata era' schema files =
  case txMetadataSupportedInEra era' of
    Nothing ->
      return . Left
        . MetadataErrorNotAvailableInEra
        $ getIsCardanoEraConstraint era' $ AnyCardanoEra era'
    Just supported -> do
      let exceptAllTxMetadata = mapM (readFileTxMetadata schema) files
      eAllTxMetaData <- runExceptT exceptAllTxMetadata
      return $ do
        metaData <- eAllTxMetaData
        Right $ TxMetadataInEra supported $ mconcat metaData

readFileTxMetadata
  :: TxMetadataJsonSchema
  -> MetadataFile
  -> ExceptT MetadataError IO TxMetadata
readFileTxMetadata mapping (MetadataFileJSON fp) = do
  bs <- handleIOExceptT (MetadataErrorFile . FileIOError fp)
          $ LBS.readFile fp
  v <- firstExceptT (MetadataErrorJsonParseError fp)
          $ hoistEither $ Aeson.eitherDecode' bs
  txMetadata <- firstExceptT (MetadataErrorConversionError fp)
                  . hoistEither $ metadataFromJson mapping v
  firstExceptT (MetadataErrorValidationError fp)
    . hoistEither $ do
      validateTxMetadata txMetadata
      return txMetadata
readFileTxMetadata _ (MetadataFileCBOR fp) = do
  bs <- handleIOExceptT (MetadataErrorFile . FileIOError fp)
          $ BS.readFile fp
  txMetadata <- firstExceptT (MetadataErrorDecodeError fp)
                  . hoistEither $ deserialiseFromCBOR AsTxMetadata bs
  firstExceptT (MetadataErrorValidationError fp)
    . hoistEither $ do
      validateTxMetadata txMetadata
      return txMetadata

-- Script witnesses/ Scripts

data ScriptWitnessError
  = ScriptWitnessErrorFile (FileError ScriptDecodeError)
  | ScriptWitnessErrorScriptLanguageNotSupportedInEra AnyScriptLanguage AnyCardanoEra
  | ScriptWitnessErrorExpectedSimple !FilePath !AnyScriptLanguage
  | ScriptWitnessErrorExpectedPlutus !FilePath !AnyScriptLanguage
  | ScriptWitnessErrorReferenceScriptsNotSupportedInEra !AnyCardanoEra
  | ScriptWitnessErrorScriptData ScriptDataError

renderScriptWitnessError :: ScriptWitnessError -> Text
renderScriptWitnessError (ScriptWitnessErrorFile err) =
  Text.pack $ displayError err
renderScriptWitnessError (ScriptWitnessErrorScriptLanguageNotSupportedInEra (AnyScriptLanguage lang) anyEra) =
  "The script language " <> Text.pack (show lang) <> " is not supported in the " <>
  renderEra anyEra <> " era'."
renderScriptWitnessError (ScriptWitnessErrorExpectedSimple file (AnyScriptLanguage lang)) =
  Text.pack $ file <> ": expected a script in the simple script language, " <>
  "but it is actually using " <> show lang <> ". Alternatively, to use " <>
  "a Plutus script, you must also specify the redeemer " <>
  "(datum if appropriate) and script execution units."
renderScriptWitnessError (ScriptWitnessErrorExpectedPlutus file (AnyScriptLanguage lang)) =
  Text.pack $ file <> ": expected a script in the Plutus script language, " <>
  "but it is actually using " <> show lang <> "."
renderScriptWitnessError (ScriptWitnessErrorReferenceScriptsNotSupportedInEra anyEra) =
  "Reference scripts not supported in era': " <> renderEra anyEra
renderScriptWitnessError (ScriptWitnessErrorScriptData sDataError) =
  renderScriptDataError sDataError

readScriptWitnessFiles
  :: CardanoEra era
  -> [(a, Maybe (ScriptWitnessFiles ctx))]
  -> ExceptT ScriptWitnessError IO [(a, Maybe (ScriptWitness ctx era))]
readScriptWitnessFiles era' = mapM readSwitFile
 where
  readSwitFile (tIn, Just switFile) = do
      sWit <- readScriptWitness era' switFile
      return (tIn, Just sWit)
  readSwitFile (tIn, Nothing) = return (tIn, Nothing)

readScriptWitnessFilesThruple
  :: CardanoEra era
  -> [(a, b, Maybe (ScriptWitnessFiles ctx))]
  -> ExceptT ScriptWitnessError IO [(a, b, Maybe (ScriptWitness ctx era))]
readScriptWitnessFilesThruple era' = mapM readSwitFile
 where
  readSwitFile (tIn, b, Just switFile) = do
      sWit <- readScriptWitness era' switFile
      return (tIn, b, Just sWit)
  readSwitFile (tIn, b, Nothing) = return (tIn, b, Nothing)

readScriptWitness
  :: CardanoEra era
  -> ScriptWitnessFiles witctx
  -> ExceptT ScriptWitnessError IO (ScriptWitness witctx era)
readScriptWitness era' (SimpleScriptWitnessFile (ScriptFile scriptFile)) = do
    script@(ScriptInAnyLang lang _) <- firstExceptT ScriptWitnessErrorFile $
                                         readFileScriptInAnyLang scriptFile
    ScriptInEra langInEra script'   <- validateScriptSupportedInEra era' script
    case script' of
      SimpleScript version sscript ->
        return . SimpleScriptWitness
                   langInEra version $ SScript sscript

      -- If the supplied cli flags were for a simple script (i.e. the user did
      -- not supply the datum, redeemer or ex units), but the script file turns
      -- out to be a valid plutus script, then we must fail.
      PlutusScript{} ->
        left $ ScriptWitnessErrorExpectedSimple
                 scriptFile
                 (AnyScriptLanguage lang)

readScriptWitness era' (PlutusScriptWitnessFiles
                          (ScriptFile scriptFile)
                          datumOrFile
                          redeemerOrFile
                          execUnits) = do
    script@(ScriptInAnyLang lang _) <- firstExceptT ScriptWitnessErrorFile $
                                         readFileScriptInAnyLang scriptFile
    ScriptInEra langInEra script'   <- validateScriptSupportedInEra era' script
    case script' of
      PlutusScript version pscript -> do
        datum <- firstExceptT ScriptWitnessErrorScriptData
                   $ readScriptDatumOrFile    datumOrFile
        redeemer <- firstExceptT ScriptWitnessErrorScriptData
                      $ readScriptRedeemerOrFile redeemerOrFile
        return $ PlutusScriptWitness
                   langInEra version (PScript pscript)
                   datum
                   redeemer
                   execUnits

      -- If the supplied cli flags were for a plutus script (i.e. the user did
      -- supply the datum, redeemer and ex units), but the script file turns
      -- out to be a valid simple script, then we must fail.
      SimpleScript{} ->
        left $ ScriptWitnessErrorExpectedPlutus
                 scriptFile
                 (AnyScriptLanguage lang)

readScriptWitness era' (PlutusReferenceScriptWitnessFiles refTxIn
                          anyScrLang@(AnyScriptLanguage anyScriptLanguage)
                          datumOrFile redeemerOrFile execUnits mPid) = do
  case refInsScriptsAndInlineDatsSupportedInEra era' of
    Nothing -> left $ ScriptWitnessErrorReferenceScriptsNotSupportedInEra
                    $ getIsCardanoEraConstraint era' (AnyCardanoEra era')
    Just _ -> do

      case scriptLanguageSupportedInEra era' anyScriptLanguage of
        Just sLangInEra ->
          case languageOfScriptLanguageInEra sLangInEra of
            SimpleScriptLanguage _v ->
              -- TODO: We likely need another datatype eg data ReferenceScriptWitness lang
              -- in order to make this branch unrepresentable.
              error "readScriptWitness: Should not be possible to specify a simple script"
            PlutusScriptLanguage version -> do
              datum <- firstExceptT ScriptWitnessErrorScriptData
                         $ readScriptDatumOrFile    datumOrFile
              redeemer <- firstExceptT ScriptWitnessErrorScriptData
                            $ readScriptRedeemerOrFile redeemerOrFile
              return $ PlutusScriptWitness
                         sLangInEra
                         version
                         (PReferenceScript refTxIn (unPolicyId <$> mPid))
                         datum redeemer execUnits
        Nothing ->
          left $ ScriptWitnessErrorScriptLanguageNotSupportedInEra anyScrLang (anyCardanoEra era')
readScriptWitness era' (SimpleReferenceScriptWitnessFiles refTxIn
                         anyScrLang@(AnyScriptLanguage anyScriptLanguage) mPid) = do
  case refInsScriptsAndInlineDatsSupportedInEra era' of
    Nothing -> left $ ScriptWitnessErrorReferenceScriptsNotSupportedInEra
                    $ getIsCardanoEraConstraint era' (AnyCardanoEra era')
    Just _ -> do
      case scriptLanguageSupportedInEra era' anyScriptLanguage of
        Just sLangInEra ->
          case languageOfScriptLanguageInEra sLangInEra of
            SimpleScriptLanguage v ->
              return . SimpleScriptWitness sLangInEra v
                     $ SReferenceScript refTxIn (unPolicyId <$> mPid)
            PlutusScriptLanguage{} ->
              error "readScriptWitness: Should not be possible to specify a plutus script"
        Nothing ->
          left $ ScriptWitnessErrorScriptLanguageNotSupportedInEra anyScrLang (anyCardanoEra era')

validateScriptSupportedInEra :: CardanoEra era
                             -> ScriptInAnyLang
                             -> ExceptT ScriptWitnessError IO (ScriptInEra era)
validateScriptSupportedInEra era' script@(ScriptInAnyLang lang _) =
    case toScriptInEra era' script of
      Nothing -> left $ ScriptWitnessErrorScriptLanguageNotSupportedInEra
                          (AnyScriptLanguage lang) (anyCardanoEra era')
      Just script' -> pure script'

data ScriptDataError =
    ScriptDataErrorFile (FileError ())
  | ScriptDataErrorJsonParse !FilePath !String
  | ScriptDataErrorConversion !FilePath !ScriptDataJsonError
  | ScriptDataErrorValidation !FilePath !ScriptDataRangeError
  | ScriptDataErrorMetadataDecode !FilePath !CBOR.DecoderError

renderScriptDataError :: ScriptDataError -> Text
renderScriptDataError (ScriptDataErrorFile err) =
  Text.pack $ displayError err
renderScriptDataError (ScriptDataErrorJsonParse fp jsonErr) =
  Text.pack $ "Invalid JSON format in file: " <> show fp <>
              "\nJSON parse error: " <> jsonErr
renderScriptDataError (ScriptDataErrorConversion fp sDataJsonErr) =
  Text.pack $ "Error reading metadata at: " <> show fp <>
              "\n" <> displayError sDataJsonErr
renderScriptDataError (ScriptDataErrorValidation fp sDataRangeErr) =
  Text.pack $ "Error validating script data at: " <> show fp <> ":\n" <>
              displayError sDataRangeErr
renderScriptDataError (ScriptDataErrorMetadataDecode fp decoderErr) =
  Text.pack $ "Error decoding CBOR metadata at: " <> show fp <>
              " Error: " <> show decoderErr

readScriptDatumOrFile :: ScriptDatumOrFile witctx
                      -> ExceptT ScriptDataError IO (ScriptDatum witctx)
readScriptDatumOrFile (ScriptDatumOrFileForTxIn df) = ScriptDatumForTxIn <$>
                                                        readScriptDataOrFile df
readScriptDatumOrFile InlineDatumPresentAtTxIn      = pure InlineScriptDatum
readScriptDatumOrFile NoScriptDatumOrFileForMint    = pure NoScriptDatumForMint
readScriptDatumOrFile NoScriptDatumOrFileForStake   = pure NoScriptDatumForStake

readScriptRedeemerOrFile :: ScriptRedeemerOrFile
                         -> ExceptT ScriptDataError IO ScriptRedeemer
readScriptRedeemerOrFile = readScriptDataOrFile

readScriptDataOrFile :: ScriptDataOrFile
                     -> ExceptT ScriptDataError IO ScriptData
readScriptDataOrFile (ScriptDataValue d) = return d
readScriptDataOrFile (ScriptDataJsonFile fp) = do
  bs <- handleIOExceptT (ScriptDataErrorFile . FileIOError fp) $ LBS.readFile fp
  v  <- firstExceptT (ScriptDataErrorJsonParse fp)
          $ hoistEither $ Aeson.eitherDecode' bs
  sd <- firstExceptT (ScriptDataErrorConversion fp)
          $ hoistEither $ scriptDataFromJson ScriptDataJsonDetailedSchema v
  firstExceptT (ScriptDataErrorValidation fp)
          $ hoistEither $ validateScriptData sd
  return sd
readScriptDataOrFile (ScriptDataCborFile fp) = do
  bs <- handleIOExceptT (ScriptDataErrorFile . FileIOError fp)
          $ BS.readFile fp
  sd <- firstExceptT (ScriptDataErrorMetadataDecode fp)
          $ hoistEither $ deserialiseFromCBOR AsScriptData bs
  firstExceptT (ScriptDataErrorValidation fp)
          $ hoistEither $ validateScriptData sd
  return sd


-- TODO: Move me

renderEra :: AnyCardanoEra -> Text
renderEra (AnyCardanoEra ByronEra)   = "Byron"
renderEra (AnyCardanoEra ShelleyEra) = "Shelley"
renderEra (AnyCardanoEra AllegraEra) = "Allegra"
renderEra (AnyCardanoEra MaryEra)    = "Mary"
renderEra (AnyCardanoEra AlonzoEra)  = "Alonzo"
renderEra (AnyCardanoEra BabbageEra) = "Babbage"
