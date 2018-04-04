{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module DeleteMerged (deleteMerged) where

import Data.Function ((&))
import qualified Data.Text as T
import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import Utils (setupNixpkgs)
import Data.Semigroup ((<>))
import Shelly

default (T.Text)


deleteMerged :: Sh ()
deleteMerged = do
    setupNixpkgs

    cmd "git" "fetch" "--prune" "origin"
    cmd "git" "fetch" "--prune" "upstream"

    cmd "git" "checkout" "master"
    cmd "git" "reset" "--hard" "upstream/master"

    mergedRemoteBranches <- T.lines <$> cmd "git" "branch" "-ra" "--merged"
    let mergedRemoteAutoUpdateBranches = mergedRemoteBranches & filter ("origin/auto-update/" `T.isInfixOf`) & mapMaybe (T.stripPrefix "remotes/origin/" . T.strip)
    forM_ mergedRemoteAutoUpdateBranches $ \branch -> do
        cmd "git" "push" "origin" (":" <> branch)

    mergedBranches <- T.lines <$> cmd "git" "branch" "-a" "--merged"
    let mergedAutoUpdateBranches = mergedBranches & filter ("auto-update/" `T.isInfixOf`)
    forM_ mergedAutoUpdateBranches $ \branch -> do
        cmd "git" "branch" "-d" branch

