{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.Script.NodeConfig
  ( startProtocol
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except.Extra

import           Cardano.Benchmarking.OuroborosImports as Core (getGenesis, protocolToNetworkId)
import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store as Store
import           Cardano.Benchmarking.Tracer

import           Cardano.TxGenerator.Setup.NodeConfig
import           Cardano.TxGenerator.Types (TxGenError)


liftToAction :: IO (Either TxGenError a) -> ActionM a
liftToAction = firstExceptT TxGenError . newExceptT . liftIO

startProtocol :: FilePath -> ActionM ()
startProtocol filePath = do
  nodeConfig <- liftToAction $ mkNodeConfig filePath
  protocol <- liftToAction $ mkConsensusProtocol nodeConfig
  set Protocol protocol
  set Genesis $ Core.getGenesis protocol
  set (User TNetworkId) $ protocolToNetworkId protocol
  liftIO initDefaultTracers >>= set Store.BenchTracers
