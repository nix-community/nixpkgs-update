{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Git
  ( cleanAndResetToMaster
  , cleanAndResetToStaging
  , cleanup
  , fetchIfStale
  , fetch
  , push
  , checkoutAtMergeBase
  , checkAutoUpdateBranchDoesn'tExist
  , commit
  , headHash
  , deleteBranch
  , showRef
  ) where

import Control.Error
import Control.Monad.Trans.Class
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Shelly
import System.Directory (getHomeDirectory, getModificationTime)
import Utils (Options(..), UpdateEnv(..), branchName, canFail)

default (T.Text)

clean :: IO ()
clean = shelly $ cmd "git" "clean" "-fdx"

cleanAndResetTo :: Text -> Text -> IO ()
cleanAndResetTo branch target = do
  shelly $ cmd "git" "reset" "--hard"
  clean
  shelly $ cmd "git" "checkout" "-B" branch target
  shelly $ cmd "git" "reset" "--hard" target
  clean

cleanAndResetToMaster :: IO ()
cleanAndResetToMaster = cleanAndResetTo "master" "upstream/master"

cleanAndResetToStaging :: IO ()
cleanAndResetToStaging = cleanAndResetTo "staging" "upstream/staging"

cleanup :: Text -> IO ()
cleanup branchName = do
  cleanAndResetToMaster
  shelly $ canFail $ cmd "git" "branch" "-D" branchName

showRef :: Text -> IO Text
showRef ref = shelly $ cmd "git" "show-ref" ref

staleFetchHead :: IO Bool
staleFetchHead = do
  home <- getHomeDirectory
  let fetchHead = home <> "/.cache/nixpkgs/.git/FETCH_HEAD"
  oneHourAgo <- addUTCTime (fromInteger $ -60 * 60) <$> getCurrentTime
  fetchedLast <- getModificationTime fetchHead
  return (fetchedLast < oneHourAgo)

fetchIfStale :: IO ()
fetchIfStale = whenM staleFetchHead fetch

fetch :: IO ()
fetch =
  shelly $ canFail $ cmd "git" "fetch" "-q" "--prune" "--multiple" "upstream" "origin"

push :: UpdateEnv -> IO ()
push updateEnv =
  shelly $ run_
    "git"
    (["push", "--force", "--set-upstream", "origin", branchName updateEnv] ++
     ["--dry-run" | dryRun (options updateEnv)])

checkoutAtMergeBase :: Text -> IO ()
checkoutAtMergeBase branchName = do
  base <-
    T.strip <$> (shelly $ cmd "git" "merge-base" "upstream/master" "upstream/staging")
  shelly $ cmd "git" "checkout" "-B" branchName base

checkAutoUpdateBranchDoesn'tExist :: Text -> ExceptT Text IO ()
checkAutoUpdateBranchDoesn'tExist packageName = do
  remoteBranches <-
    lift $ map T.strip . T.lines <$> (shelly $ silently $ cmd "git" "branch" "--remote")
  when
    (("origin/auto-update/" <> packageName) `elem` remoteBranches)
    (throwE "Update branch already on origin.")

commit :: Text -> IO ()
commit ref = shelly $ cmd "git" "commit" "-am" ref

headHash :: IO Text
headHash = shelly $ cmd "git" "rev-parse" "HEAD"

deleteBranch :: Text -> IO ()
deleteBranch branchName = shelly $ do
  canFail $ do
    cmd "git" "branch" "-D" branchName
    cmd "git" "push" "origin" (":" <> branchName)
