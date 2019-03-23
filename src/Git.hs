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
import System.Process.Typed
import Utils (Options(..), UpdateEnv(..), branchName)

default (T.Text)

clean :: MonadIO m => m ()
clean = runProcess_ "git clean -fdx"

checkout :: MonadIO m => Text -> Text -> m ()
checkout branch target =
  runProcess_ (proc "git" ["checkout", "-B", T.unpack branch, T.unpack target])

reset :: MonadIO m => Text -> m ()
reset target = runProcess_ (proc "git" ["reset", "--hard", T.unpack target])

delete :: Text -> ProcessConfig () () ()
delete branch = proc "git" ["branch", "-D", T.unpack branch]

cleanAndResetTo :: MonadIO m => Text -> m ()
cleanAndResetTo branch =
  let target = "upstream/" <> branch
   in do runProcess_ "git reset --hard"
         clean
         checkout branch target
         reset target
         clean

cleanup :: MonadIO m => Text -> m ()
cleanup bName = do
  cleanAndResetTo "master"
  void $ runProcess (delete bName)

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

-- Using void and runProcess because if this fails we want to keep
-- going
fetch :: MonadIO m => m ()
fetch = void $ runProcess "git fetch -q --prune --multiple upstream origin"

push :: MonadIO m => UpdateEnv -> ExceptT Text m ()
push updateEnv =
  Shell.shellyET $
  run_
    "git"
    (["push", "--force", "--set-upstream", "origin", branchName updateEnv] ++
     ["--dry-run" | dryRun (options updateEnv)])

checkoutAtMergeBase :: MonadIO m => Text -> ExceptT Text m ()
checkoutAtMergeBase bName = do
  base <-
    T.strip <$>
    Shell.shellyET (cmd "git" "merge-base" "upstream/master" "upstream/staging")
  Shell.shellyET $ cmd "git" "checkout" "-B" bName base

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
