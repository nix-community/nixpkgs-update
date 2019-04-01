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

import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Directory (getHomeDirectory, getModificationTime)
import System.Exit
import System.Process.Typed
import Utils (Options(..), UpdateEnv(..), branchName)

clean :: ProcessConfig () () ()
clean = silently "git clean -fdx"

checkout :: Text -> Text -> ProcessConfig () () ()
checkout branch target =
  silently $ proc "git" ["checkout", "-B", T.unpack branch, T.unpack target]

reset :: Text -> ProcessConfig () () ()
reset target = silently $ proc "git" ["reset", "--hard", T.unpack target]

delete :: Text -> ProcessConfig () () ()
delete branch = silently $ proc "git" ["branch", "-D", T.unpack branch]

deleteOrigin :: Text -> ProcessConfig () () ()
deleteOrigin branch =
  silently $ proc "git" ["push", "origin", T.unpack (":" <> branch)]

cleanAndResetTo :: MonadIO m => Text -> ExceptT Text m ()
cleanAndResetTo branch =
  let target = "upstream/" <> branch
   in do runProcessNoIndexIssue_ $ silently "git reset --hard"
         runProcessNoIndexIssue_ clean
         runProcessNoIndexIssue_ $ checkout branch target
         runProcessNoIndexIssue_ $ reset target
         runProcessNoIndexIssue_ clean

cleanup :: MonadIO m => Text -> ExceptT Text m ()
cleanup bName = do
  liftIO $ T.putStrLn ("Cleaning up " <> bName)
  cleanAndResetTo "master"
  runProcessNoIndexIssue_ (delete bName) <|>
    (liftIO $ T.putStrLn ("Couldn't delete " <> bName))

staleFetchHead :: MonadIO m => m Bool
staleFetchHead =
  liftIO $ do
    home <- getHomeDirectory
    let fetchHead = home <> "/.cache/nixpkgs/.git/FETCH_HEAD"
    oneHourAgo <- addUTCTime (fromInteger $ -60 * 60) <$> getCurrentTime
    fetchedLast <- getModificationTime fetchHead
    return (fetchedLast < oneHourAgo)

fetchIfStale :: MonadIO m => ExceptT Text m ()
fetchIfStale = whenM staleFetchHead fetch

fetch :: MonadIO m => ExceptT Text m ()
fetch =
  runProcessNoIndexIssue_ $
  silently "git fetch -q --prune --multiple upstream origin"

push :: MonadIO m => UpdateEnv -> ExceptT Text m ()
push updateEnv = do
  runProcessNoIndexIssue_
    (proc
       "git"
       ([ "push"
        , "--force"
        , "--set-upstream"
        , "origin"
        , T.unpack (branchName updateEnv)
        ] ++
        ["--dry-run" | dryRun (options updateEnv)]))

checkoutAtMergeBase :: MonadIO m => Text -> ExceptT Text m ()
checkoutAtMergeBase bName = do
  base <-
    readProcessInterleavedNoIndexIssue_
      "git merge-base upstream/master upstream/staging" &
    fmapRT T.strip
  runProcessNoIndexIssue_ (checkout bName base)

checkAutoUpdateBranchDoesntExist :: MonadIO m => Text -> ExceptT Text m ()
checkAutoUpdateBranchDoesntExist pName = do
  remoteBranches <-
    readProcessInterleavedNoIndexIssue_ "git branch --remote" &
    fmapRT (T.lines >>> fmap T.strip)
  when
    (("origin/auto-update/" <> pName) `elem` remoteBranches)
    (throwE "Update branch already on origin. ")

commit :: MonadIO m => Text -> ExceptT Text m ()
commit ref =
  runProcessNoIndexIssue_ (proc "git" ["commit", "-am", T.unpack ref])

headHash :: MonadIO m => ExceptT Text m Text
headHash = readProcessInterleavedNoIndexIssue_ "git rev-parse HEAD"

deleteBranchEverywhere :: MonadIO m => Text -> ExceptT Text m ()
deleteBranchEverywhere bName = do
  runProcessNoIndexIssue_ $ delete bName
  runProcessNoIndexIssue_ $ deleteOrigin bName

runProcessNoIndexIssue_ ::
     MonadIO m => ProcessConfig () () () -> ExceptT Text m ()
runProcessNoIndexIssue_ config = tryIOTextET go
  where
    go = do
      (code, out, e) <- readProcess config
      case code of
        ExitFailure 128
          | "index.lock" `BS.isInfixOf` (BSL.toStrict e) -> do
            threadDelay 100000
            go
        ExitSuccess -> return ()
        ExitFailure _ -> throw $ ExitCodeException code config out e

readProcessInterleavedNoIndexIssue_ ::
     MonadIO m => ProcessConfig () () () -> ExceptT Text m Text
readProcessInterleavedNoIndexIssue_ config = tryIOTextET go
  where
    go = do
      (code, out) <- readProcessInterleaved config
      case code of
        ExitFailure 128
          | "index.lock" `BS.isInfixOf` (BSL.toStrict out) -> do
            threadDelay 100000
            go
        ExitSuccess -> return $ (BSL.toStrict >>> T.decodeUtf8) out
        ExitFailure _ -> throw $ ExitCodeException code config out out
