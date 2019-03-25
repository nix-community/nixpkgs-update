{-# LANGUAGE OverloadedStrings #-}

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
  , deleteBranchEverywhere
  ) where

import OurPrelude

import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Directory (getHomeDirectory, getModificationTime)
import System.Posix.Files (fileExist)
import System.Process.Typed
import Utils (Options(..), UpdateEnv(..), branchName)

clean :: ProcessConfig () () ()
clean = setStdin closed $ setStdout closed $ setStderr closed $ "git clean -fdx"

checkout :: Text -> Text -> ProcessConfig () () ()
checkout branch target =
  setStdin closed $
  setStdout closed $
  setStderr closed $
  proc "git" ["checkout", "-B", T.unpack branch, T.unpack target]

reset :: Text -> ProcessConfig () () ()
reset target =
  setStdin closed $
  setStdout closed $
  setStderr closed $ proc "git" ["reset", "--hard", T.unpack target]

delete :: Text -> ProcessConfig () () ()
delete branch =
  setStdin closed $
  setStdout closed $
  setStderr closed $ proc "git" ["branch", "-D", T.unpack branch]

deleteOrigin :: Text -> ProcessConfig () () ()
deleteOrigin branch =
  setStdin closed $
  setStdout closed $
  setStderr closed $ proc "git" ["push", "origin", T.unpack (":" <> branch)]

cleanAndResetTo :: MonadIO m => Text -> m ()
cleanAndResetTo branch =
  let target = "upstream/" <> branch
   in do runProcess_ $
           setStdin closed $
           setStdout closed $ setStderr closed $ "git reset --hard"
         waitForNoLock
         runProcess_ clean
         waitForNoLock
         runProcess_ $ checkout branch target
         waitForNoLock
         runProcess_ $ reset target
         waitForNoLock
         runProcess_ clean

cleanup :: MonadIO m => Text -> m ()
cleanup bName = do
  liftIO $ T.putStrLn ("Cleaning up " <> bName)
  cleanAndResetTo "master"
  void $ runProcess (delete bName)

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
fetch =
  void $
  runProcess $
  setStdin closed $
  setStdout closed $
  setStderr closed $ "git fetch -q --prune --multiple upstream origin"

push :: MonadIO m => UpdateEnv -> ExceptT Text m ()
push updateEnv =
  runProcess_
    (proc
       "git"
       ([ "push"
        , "--force"
        , "--set-upstream"
        , "origin"
        , T.unpack (branchName updateEnv)
        ] ++
        ["--dry-run" | dryRun (options updateEnv)])) &
  tryIOTextET

checkoutAtMergeBase :: MonadIO m => Text -> ExceptT Text m ()
checkoutAtMergeBase bName = do
  waitForNoLock
  base <-
    ourReadProcessInterleaved_ "git merge-base upstream/master upstream/staging" &
    fmapRT T.strip
  waitForNoLock
  runProcess_ (checkout bName base) & tryIOTextET

checkAutoUpdateBranchDoesntExist :: MonadIO m => Text -> ExceptT Text m ()
checkAutoUpdateBranchDoesntExist pName = do
  remoteBranches <-
    ourReadProcessInterleaved_ "git branch --remote" &
    fmapRT (T.lines >>> fmap T.strip)
  when
    (("origin/auto-update/" <> pName) `elem` remoteBranches)
    (throwE "Update branch already on origin. ")

commit :: MonadIO m => Text -> ExceptT Text m ()
commit ref =
  (runProcess_ (proc "git" ["commit", "-am", T.unpack ref])) & tryIOTextET

headHash :: MonadIO m => ExceptT Text m Text
headHash = ourReadProcessInterleaved_ "git rev-parse HEAD"

deleteBranchEverywhere :: MonadIO m => Text -> m ()
deleteBranchEverywhere bName = do
  void $ runProcess (delete bName)
  void $ runProcess (deleteOrigin bName)

waitForNoLock :: MonadIO m => m ()
waitForNoLock = do
  liftIO $
    whenM (fileExist ".git/index.lock") $ do
      threadDelay 10000
      waitForNoLock
