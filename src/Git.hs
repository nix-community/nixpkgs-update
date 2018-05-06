{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Git
  ( cleanAndResetToMaster
  , cleanAndResetToStaging
  , cleanup
  , fetchIfStale
  , push
  , checkoutAtMergeBase
  , autoUpdateBranchExists
  , commit
  , pr
  ) where

import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Shelly
import System.Directory (getModificationTime)
import Utils (Options(..), UpdateEnv(..), branchName, canFail)

default (T.Text)

clean :: Sh ()
clean = cmd "git" "clean" "-fdx"

cleanAndResetTo :: Text -> Text -> Sh ()
cleanAndResetTo branch target = do
  cmd "git" "reset" "--hard"
  clean
  cmd "git" "checkout" "-B" branch target
  cmd "git" "reset" "--hard" target
  clean

cleanAndResetToMaster :: Sh ()
cleanAndResetToMaster = cleanAndResetTo "master" "upstream/master"

cleanAndResetToStaging :: Sh ()
cleanAndResetToStaging = cleanAndResetTo "staging" "upstream/staging"

cleanup :: Text -> Sh ()
cleanup branchName = do
  cleanAndResetToMaster
  canFail $ cmd "git" "branch" "-D" branchName

staleFetchHead :: IO Bool
staleFetchHead = do
  oneHourAgo <- addUTCTime (fromInteger $ -60 * 60) <$> getCurrentTime
  fetchedLast <- getModificationTime ".git/FETCH_HEAD"
  return (fetchedLast < oneHourAgo)

fetchIfStale :: Sh ()
fetchIfStale =
  whenM
    (liftIO staleFetchHead)
    (canFail $ cmd "git" "fetch" "--prune" "--multiple" "upstream" "origin")

push :: UpdateEnv -> Sh ()
push updateEnv =
  run_
    "git"
    (["push", "--force", "--set-upstream", "origin", branchName updateEnv] ++
     ["--dry-run" | dryRun (options updateEnv)])

checkoutAtMergeBase :: Text -> Sh ()
checkoutAtMergeBase branchName = do
  base <-
    T.strip <$> cmd "git" "merge-base" "upstream/master" "upstream/staging"
  cmd "git" "checkout" "-B" branchName base

autoUpdateBranchExists :: Text -> Sh Bool
autoUpdateBranchExists packageName = do
  remotes <- map T.strip . T.lines <$> cmd "git" "branch" "--remote"
  return $ ("origin/auto-update/" <> packageName) `elem` remotes

commit :: Text -> Sh ()
commit commitMessage = cmd "git" "commit" "-am" commitMessage

pr :: Text -> Sh ()
pr prMessage = cmd "hub" "pull-request" "-m" prMessage
