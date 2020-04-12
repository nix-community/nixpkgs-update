{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Process where

import qualified Data.ByteString.Lazy as BSL
import Polysemy
import Polysemy.Input
import qualified System.Process.Typed as TP
import System.Exit (ExitCode(..))

data Process m a where
  ReadInterleaved_ :: TP.ProcessConfig stdin stdout stderr -> Process m BSL.ByteString
  ReadInterleaved :: TP.ProcessConfig stdin stdout stderr -> Process m (ExitCode, BSL.ByteString)

makeSem ''Process

runIO ::
  Member (Embed IO) r =>
  Sem (Process ': r) a ->
  Sem r a
runIO =
  interpret $ \case
    ReadInterleaved_ config -> embed $ (TP.readProcessInterleaved_ config)
    ReadInterleaved config -> embed $ (TP.readProcessInterleaved config)

runPure ::
  [BSL.ByteString] ->
  Sem (Process ': r) a ->
  Sem r a
runPure outputList =
  runInputList outputList
    . reinterpret \case
      ReadInterleaved_ _config -> maybe "" id <$> input
      ReadInterleaved _config -> do
        r <- maybe "" id <$> input
        return (ExitSuccess, r)
