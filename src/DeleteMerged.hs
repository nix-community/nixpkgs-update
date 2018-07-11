{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module DeleteMerged
  ( deleteMerged
  ) where

import Control.Monad (forM_)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Shelly
import Utils (ourShell, Options)

default (T.Text)

-- | Delete the already merged branches both from local and remote repository
deleteMerged :: Options -> IO ()
deleteMerged o = ourShell o $ do
  cmd "git" "fetch" "--prune" "origin"
  cmd "git" "fetch" "--prune" "upstream"
  cmd "git" "checkout" "master"
  cmd "git" "reset" "--hard" "upstream/master"
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
