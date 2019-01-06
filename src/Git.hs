{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Git
  ( cleanAndResetTo
  , cleanup
  , fetchIfStale
  , fetch
  , push
  , checkoutAtMergeBase
  , checkAutoUpdateBranchDoesntExist
  , commit
  , headHash
  , deleteBranch
  , showRef
  ) where

import OurPrelude

import qualified Data.Text as T
import Data.Time.Clock (addUTCTime, getCurrentTime)
import qualified Shell
import Shelly
import System.Directory (getHomeDirectory, getModificationTime)
import Utils (Options(..), UpdateEnv(..), branchName)

default (T.Text)

clean :: MonadIO m => m ()
clean = shelly $ cmd "git" "clean" "-fdx"

cleanAndResetTo :: MonadIO m => Text -> m ()
cleanAndResetTo branch =
  let target = "upstream/" <> branch
   in do _ <- shelly $ cmd "git" "reset" "--hard"
         clean
         _ <- shelly $ cmd "git" "checkout" "-B" branch target
         _ <- shelly $ cmd "git" "reset" "--hard" target
         clean

cleanup :: MonadIO m => Text -> m ()
cleanup bName = do
  cleanAndResetTo "master"
  shelly $ Shell.canFail $ cmd "git" "branch" "-D" bName

showRef :: MonadIO m => Text -> m Text
showRef ref = shelly $ cmd "git" "show-ref" ref

staleFetchHead :: MonadIO m => m Bool
staleFetchHead =
  liftIO $ do
    home <- getHomeDirectory
    let fetchHead = home <> "/.cache/nixpkgs/.git/FETCH_HEAD"
    oneHourAgo <- addUTCTime (fromInteger $ -60 * 60) <$> getCurrentTime
    fetchedLast <- getModificationTime fetchHead
    return (fetchedLast < oneHourAgo)

fetchIfStale :: MonadIO m => m ()
fetchIfStale = whenM staleFetchHead fetch

fetch :: MonadIO m => m ()
fetch =
  shelly $
  Shell.canFail $
  cmd "git" "fetch" "-q" "--prune" "--multiple" "upstream" "origin"

push :: MonadIO m => UpdateEnv -> ExceptT Text m ()
push updateEnv =
  Shell.shellyET $
  run_
    "git"
    (["push", "--force", "--set-upstream", "origin", branchName updateEnv] ++
     ["--dry-run" | dryRun (options updateEnv)])

checkoutAtMergeBase :: MonadIO m => Text -> m ()
checkoutAtMergeBase bName = do
  base <-
    T.strip <$>
    shelly (cmd "git" "merge-base" "upstream/master" "upstream/staging")
  shelly $ cmd "git" "checkout" "-B" bName base

checkAutoUpdateBranchDoesntExist :: MonadIO m => Text -> ExceptT Text m ()
checkAutoUpdateBranchDoesntExist pName = do
  remoteBranches <-
    lift $
    map T.strip . T.lines <$> shelly (silently $ cmd "git" "branch" "--remote")
  when
    (("origin/auto-update/" <> pName) `elem` remoteBranches)
    (throwE "Update branch already on origin.")

commit :: MonadIO m => Text -> ExceptT Text m ()
commit ref = Shell.shellyET $ cmd "git" "commit" "-am" ref

headHash :: MonadIO m => ExceptT Text m Text
headHash = Shell.shellyET $ cmd "git" "rev-parse" "HEAD"

deleteBranch :: MonadIO m => Text -> m ()
deleteBranch bName =
  shelly $
  Shell.canFail $ do
    _ <- cmd "git" "branch" "-D" bName
    cmd "git" "push" "origin" (":" <> bName)
