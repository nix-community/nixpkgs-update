{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Process where

import qualified Data.ByteString.Lazy as BSL
import Polysemy
import Polysemy.Input
import System.Exit (ExitCode (..))
import qualified System.Process.Typed as TP

data Process m a where
  Read_ :: TP.ProcessConfig stdin stdout stderr -> Process m (BSL.ByteString, BSL.ByteString)
  ReadInterleaved_ :: TP.ProcessConfig stdin stdout stderr -> Process m BSL.ByteString
  ReadInterleaved :: TP.ProcessConfig stdin stdout stderr -> Process m (ExitCode, BSL.ByteString)

makeSem ''Process

runIO ::
  (Member (Embed IO) r) =>
  Sem (Process ': r) a ->
  Sem r a
runIO =
  interpret $ \case
    Read_ config -> embed $ (TP.readProcess_ config)
    ReadInterleaved_ config -> embed $ (TP.readProcessInterleaved_ config)
    ReadInterleaved config -> embed $ (TP.readProcessInterleaved config)

runPure ::
  [BSL.ByteString] ->
  Sem (Process ': r) a ->
  Sem r a
runPure outputList =
  runInputList outputList
    . reinterpret \case
      Read_ _config -> do
        r <- maybe "" id <$> input
        return (r, "")
      ReadInterleaved_ _config -> maybe "" id <$> input
      ReadInterleaved _config -> do
        r <- maybe "" id <$> input
        return (ExitSuccess, r)
