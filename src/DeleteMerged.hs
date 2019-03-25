{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module DeleteMerged
  ( deleteDone
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified GH
import qualified Git
import Utils (Options)

default (T.Text)

deleteDone :: Options -> IO ()
deleteDone o = do
  Git.fetch
  Git.cleanAndResetTo "master"
  result <- GH.closedAutoUpdateRefs o
  case result of
    Left e -> T.putStrLn e
    Right refs ->
      V.sequence_
        (fmap (\r -> Git.deleteBranchEverywhere ("auto-update/" <> r)) refs)
