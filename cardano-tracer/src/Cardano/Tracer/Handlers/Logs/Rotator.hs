{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tracer.Handlers.Logs.Rotator
  ( runLogsRotator
  ) where

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad (forM_, forever, unless, when)
import           Control.Monad.Extra (whenJust, whenM)
import           Data.List (nub, sort)
import           Data.List.Extra (dropEnd)
import qualified Data.List.NonEmpty as NE
import           Data.Time (diffUTCTime, getCurrentTime)
import           Data.Word (Word64)
import           System.Directory (doesDirectoryExist, getFileSize, removeFile)
import           System.Directory.Extra (listDirectories, listFiles)
import           System.FilePath ((</>), takeDirectory)
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Utils (createLogAndUpdateSymLink,
                   getTimeStampFromLog, isItLog)
import           Cardano.Tracer.Utils (showProblemIfAny)

-- | Runs rotation mechanism for the log files.
runLogsRotator :: TracerConfig -> IO ()
runLogsRotator TracerConfig{rotation, logging, verbosity} =
  whenJust rotation $ \rotParams ->
    launchRotator loggingParamsForFiles rotParams verbosity
 where
  loggingParamsForFiles = nub . NE.filter filesOnly $ logging
  filesOnly LoggingParams{logMode} = logMode == FileMode

launchRotator
  :: [LoggingParams]
  -> RotationParams
  -> Maybe Verbosity
  -> IO ()
launchRotator [] _ _ = return ()
launchRotator loggingParamsForFiles rotParams@RotationParams{rpFrequencySecs} verb = forever $ do
  showProblemIfAny verb $
    forM_ loggingParamsForFiles $ checkRootDir rotParams
  sleep $ fromIntegral rpFrequencySecs

-- | All the logs with 'TraceObject's received from particular node
--   will be stored in a separate subdirectory in the root directory.
--
--   Each subdirectory contains a symbolic link, we use it to write
--   log items to the latest log file. When we create the new log file,
--   this symbolic link is switched to it.
checkRootDir
  :: RotationParams
  -> LoggingParams
  -> IO ()
checkRootDir rotParams LoggingParams{logRoot, logFormat} =
  whenM (doesDirectoryExist logRoot) $
    listDirectories logRoot >>= \case
      [] ->
        -- There are no nodes' subdirs yet (or they were deleted),
        -- so no rotation can be performed for now.
        return ()
      logsSubDirs -> do
        let fullPathsToSubDirs = map (logRoot </>) logsSubDirs
        forConcurrently_ fullPathsToSubDirs $ checkLogs rotParams logFormat

-- | We check the log files:
--   1. If there are too big log files.
--   2. If there are too old log files.
checkLogs
  :: RotationParams
  -> LogFormat
  -> FilePath
  -> IO ()
checkLogs RotationParams{rpLogLimitBytes, rpMaxAgeHours, rpKeepFilesNum} format subDirForLogs = do
  logs <- map (subDirForLogs </>) . filter (isItLog format) <$> listFiles subDirForLogs
  checkIfCurrentLogIsFull logs format rpLogLimitBytes
  checkIfThereAreOldLogs logs rpMaxAgeHours rpKeepFilesNum

-- | If the current log file is full (it's size is too big),
--   the new log will be created.
checkIfCurrentLogIsFull
  :: [FilePath]
  -> LogFormat
  -> Word64
  -> IO ()
checkIfCurrentLogIsFull [] _ _ = return ()
checkIfCurrentLogIsFull logs format maxSizeInBytes =
  whenM (logIsFull pathToCurrentLog) $
    createLogAndUpdateSymLink (takeDirectory pathToCurrentLog) format
 where
  logIsFull logName = do
    size <- getFileSize logName
    return $ fromIntegral size >= maxSizeInBytes
  -- Since logs' names contain timestamps, the maximum one is the latest log,
  -- or current log (i.e. the log we write 'TraceObject's in).
  pathToCurrentLog = maximum logs

-- | If there are too old log files - they will be removed.
--   Please note that some number of log files can be kept in any case.
checkIfThereAreOldLogs
  :: [FilePath]
  -> Word
  -> Word
  -> IO ()
checkIfThereAreOldLogs [] _ _ = return ()
-- If there is one single log file, we assume that it's a current log,
-- so we cannot remove it even if it's too old.
checkIfThereAreOldLogs [_] _ _ = return ()
checkIfThereAreOldLogs logs maxAgeInHours keepFilesNum = do
  -- Logs' names contain timestamp, so we can sort them.
  let fromOldestToNewest = sort logs
      -- N ('keepFilesNum') newest files have to be kept in any case.
      logsWeHaveToCheck = dropEnd (fromIntegral keepFilesNum) fromOldestToNewest
  unless (null logsWeHaveToCheck) $ do
    now <- getCurrentTime
    checkOldLogs now logsWeHaveToCheck
 where
  checkOldLogs _ [] = return ()
  checkOldLogs now' (oldestLog:otherLogs) =
    case getTimeStampFromLog oldestLog of
      Just ts -> do
        let oldestLogAge = toSeconds $ now' `diffUTCTime` ts
        when (oldestLogAge >= maxAgeInSecs) $ do
          removeFile oldestLog
          checkOldLogs now' otherLogs
        -- If 'oldestLog' isn't outdated (yet), other logs aren't
        -- outdated too (because they are newer), so we shouldn't check them.
      Nothing ->
        -- Something is wrong with log's name, continue.
        checkOldLogs now' otherLogs

  maxAgeInSecs = fromIntegral maxAgeInHours * 3600
  toSeconds age = fromEnum age `div` 1000000000000
