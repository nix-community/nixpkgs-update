{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module DeleteMerged
  ( deleteDone
  ) where

import Control.Monad (forM_)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Git
import qualified GH
import Shelly
import Utils (Options, ourShell)
import qualified Data.Vector as V

default (T.Text)

deleteDone :: Options -> IO ()
deleteDone o = do
  Git.fetch
  Git.cleanAndResetToMaster
  result <- GH.closedAutoUpdateRefs o
  case result of
    Left error -> T.putStrLn error
    Right refs ->
      V.sequence_ (fmap (\r -> Git.deleteBranch ("auto-update/" <> r)) refs)
