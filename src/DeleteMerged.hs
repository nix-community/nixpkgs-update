{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module DeleteMerged
  ( deleteMerged
  , deleteDone
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

-- | Delete the already merged branches both from local and remote repository
deleteMerged :: Options -> IO ()
deleteMerged o = do
  ourShell o $ do
    Git.fetch
    Git.cleanAndResetToMaster
    mergedRemoteBranches <- T.lines <$> cmd "git" "branch" "-ra" "--merged"
    let mergedRemoteAutoUpdateBranches =
          mergedRemoteBranches & filter ("origin/auto-update/" `T.isInfixOf`) &
          mapMaybe (T.stripPrefix "remotes/origin/" . T.strip)
    forM_ mergedRemoteAutoUpdateBranches $ \branch ->
      cmd "git" "push" "origin" (":" <> branch)
    mergedBranches <- T.lines <$> cmd "git" "branch" "-a" "--merged"
    let mergedAutoUpdateBranches =
          mergedBranches & filter ("auto-update/" `T.isInfixOf`) & map T.strip
    forM_ mergedAutoUpdateBranches $ \branch -> cmd "git" "branch" "-d" branch

deleteDone :: Options -> IO ()
deleteDone o = do
  ourShell o $ do
    Git.fetch
    Git.cleanAndResetToMaster
    result <- liftIO $ GH.closedAutoUpdateRefs o
    case result of
      Left error -> liftIO $ T.putStrLn error
      Right refs ->
        V.sequence_ (fmap (\r -> Git.deleteBranch ("auto-update/" <> r)) refs)
