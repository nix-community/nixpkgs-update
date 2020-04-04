{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Process where

import qualified Data.ByteString.Lazy as BSL
import Polysemy
import Polysemy.Input
import qualified System.Process.Typed as TP

data Process m a where
  ReadInterleaved :: TP.ProcessConfig stdin stdout stderr -> Process m BSL.ByteString

makeSem ''Process

runIO ::
  Member (Embed IO) r =>
  Sem (Process ': r) a ->
  Sem r a
runIO =
  interpret $ \case
    ReadInterleaved config -> embed $ (TP.readProcessInterleaved_ config :: IO BSL.ByteString)

runPure ::
  [BSL.ByteString] ->
  Sem (Process ': r) a ->
  Sem r a
runPure outputList =
  runInputList outputList
    . reinterpret \case
      ReadInterleaved _config -> maybe "" id <$> input
