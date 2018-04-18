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
  ) where

import Shelly
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Semigroup ((<>))
import System.Directory (getModificationTime)
import Utils (canFail, Options(..))

default (T.Text)

cleanAndResetTo :: Text -> Text -> Sh ()
cleanAndResetTo branch target = do
  cmd "git" "reset" "--hard"
  cmd "git" "clean" "-fd"
  cmd "git" "checkout" "-B" branch target
  cmd "git" "reset" "--hard" target
  cmd "git" "clean" "-fd"

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
fetchIfStale = do
  stale <- liftIO $ staleFetchHead
  when stale $ do
    canFail $ cmd "git" "fetch" "--prune" "--multiple" "upstream" "origin"

push :: Text -> Options -> Sh ()
push branchName options =
  run_
    "git"
    (["push", "--force", "--set-upstream", "origin", branchName] ++
     if dryRun options
       then ["--dry-run"]
       else [])

checkoutAtMergeBase :: Text -> Sh ()
checkoutAtMergeBase branchName = do
  base <- T.strip <$>
    cmd "git" "merge-base" "upstream/master" "upstream/staging"
  cmd "git" "checkout" "-B" branchName base

autoUpdateBranchExists :: Text -> Sh Bool
autoUpdateBranchExists packageName = do
  remotes <- map T.strip . T.lines <$> cmd "git" "branch" "--remote"
  return $ ("origin/auto-update/" <> packageName) `elem` remotes
