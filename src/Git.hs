{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Git
  ( checkAutoUpdateBranchDoesntExist,
    checkoutAtMergeBase,
    cleanAndResetTo,
    cleanup,
    commit,
    deleteBranchesEverywhere,
    diff,
    fetch,
    fetchIfStale,
    headHash,
    push,
    nixpkgsDir,
    setupNixpkgs,
    Git.show,
  )
where

import Control.Concurrent
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (addUTCTime, getCurrentTime)
import qualified Data.Vector as V
import Language.Haskell.TH.Env (envQ)
import OurPrelude hiding (throw)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, getCurrentDirectory, setCurrentDirectory)
import System.Environment (getEnv)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Exit
import System.IO.Error (tryIOError)
import System.Posix.Env (setEnv)
import qualified System.Process.Typed
import Utils (Options (..), UpdateEnv (..), branchName, branchPrefix)

bin :: String
bin = fromJust ($$(envQ "GIT") :: Maybe String) <> "/bin/git"

procGit :: [String] -> ProcessConfig () () ()
procGit = proc bin

hubBin :: String
hubBin = fromJust ($$(envQ "HUB") :: Maybe String) <> "/bin/hub"

procHub :: [String] -> ProcessConfig () () ()
procHub = proc hubBin

clean :: ProcessConfig () () ()
clean = silently $ procGit ["clean", "-fdx"]

checkout :: Text -> Text -> ProcessConfig () () ()
checkout branch target =
  silently $ procGit ["checkout", "-B", T.unpack branch, T.unpack target]

reset :: Text -> ProcessConfig () () ()
reset target = silently $ procGit ["reset", "--hard", T.unpack target]

delete1 :: Text -> ProcessConfig () () ()
delete1 branch = delete [branch]

delete :: [Text] -> ProcessConfig () () ()
delete branches = silently $ procGit (["branch", "-D"] ++ fmap T.unpack branches)

deleteOrigin :: [Text] -> ProcessConfig () () ()
deleteOrigin branches =
  silently $ procGit (["push", "origin", "--delete"] ++ fmap T.unpack branches)

cleanAndResetTo :: MonadIO m => Text -> ExceptT Text m ()
cleanAndResetTo branch =
  let target = "upstream/" <> branch
   in do
        runProcessNoIndexIssue_ $ silently $ procGit ["reset", "--hard"]
        runProcessNoIndexIssue_ clean
        runProcessNoIndexIssue_ $ checkout branch target
        runProcessNoIndexIssue_ $ reset target
        runProcessNoIndexIssue_ clean

show :: MonadIO m => Text -> Text -> ExceptT Text m Text
show branch file =
  readProcessInterleavedNoIndexIssue_ $ silently $ procGit ["show", T.unpack ("remotes/upstream/" <> branch <> ":" <> file)]

cleanup :: MonadIO m => Text -> ExceptT Text m ()
cleanup bName = do
  liftIO $ T.putStrLn ("Cleaning up " <> bName)
  cleanAndResetTo "master"
  runProcessNoIndexIssue_ (delete1 bName)
    <|> liftIO (T.putStrLn ("Couldn't delete " <> bName))

diff :: MonadIO m => Text -> ExceptT Text m Text
diff branch = readProcessInterleavedNoIndexIssue_ $ procGit ["diff", T.unpack branch]

staleFetchHead :: MonadIO m => m Bool
staleFetchHead =
  liftIO $ do
    nixpkgsGit <- getUserCacheDir "nixpkgs"
    let fetchHead = nixpkgsGit <> "/.git/FETCH_HEAD"
    oneHourAgo <- addUTCTime (fromInteger $ -60 * 60) <$> getCurrentTime
    e <- tryIOError $ getModificationTime fetchHead
    if isLeft e
      then do
        return True
      else do
        fetchedLast <- getModificationTime fetchHead
        return (fetchedLast < oneHourAgo)

fetchIfStale :: MonadIO m => ExceptT Text m ()
fetchIfStale = whenM staleFetchHead fetch

fetch :: MonadIO m => ExceptT Text m ()
fetch =
  runProcessNoIndexIssue_ $
    silently $ procGit ["fetch", "-q", "--prune", "--multiple", "upstream", "origin"]

push :: MonadIO m => UpdateEnv -> ExceptT Text m ()
push updateEnv =
  runProcessNoIndexIssue_
    ( procGit
        ( [ "push",
            "--force",
            "--set-upstream",
            "origin",
            T.unpack (branchName updateEnv)
          ]
            ++ ["--dry-run" | not (doPR (options updateEnv))]
        )
    )

nixpkgsDir :: IO FilePath
nixpkgsDir = do
  inNixpkgs <- inNixpkgsRepo
  if inNixpkgs
    then getCurrentDirectory
    else getUserCacheDir "nixpkgs"

-- Setup a NixPkgs clone in $XDG_CACHE_DIR/nixpkgs
-- Since we are going to have to fetch, git reset, clean, and commit, we setup a
-- cache dir to avoid destroying any uncommitted work the user may have in PWD.
setupNixpkgs :: Text -> IO ()
setupNixpkgs githubt = do
  fp <- nixpkgsDir
  exists <- doesDirectoryExist fp
  unless exists $ do
    path <- getEnv "PATH"
    procHub ["clone", "nixpkgs", fp]
      & System.Process.Typed.setEnv -- requires that user has forked nixpkgs
        [ ("PATH" :: String, path),
          ("GITHUB_TOKEN" :: String, githubt & T.unpack)
        ]
      & runProcess_
    setCurrentDirectory fp
    shell (bin <> "remote add upstream https://github.com/NixOS/nixpkgs")
      & runProcess_
  inNixpkgs <- inNixpkgsRepo
  unless inNixpkgs do
    setCurrentDirectory fp
    _ <- runExceptT fetchIfStale
    _ <- runExceptT $ cleanAndResetTo "master"
    return ()
  System.Posix.Env.setEnv "NIX_PATH" ("nixpkgs=" <> fp) True

checkoutAtMergeBase :: MonadIO m => Text -> ExceptT Text m Text
checkoutAtMergeBase bName = do
  base <-
    readProcessInterleavedNoIndexIssue_
      (procGit ["merge-base", "upstream/master", "upstream/staging"])
      & fmapRT T.strip
  runProcessNoIndexIssue_ (checkout bName base)
  return base

checkAutoUpdateBranchDoesntExist :: MonadIO m => Text -> ExceptT Text m ()
checkAutoUpdateBranchDoesntExist pName = do
  remoteBranches <-
    readProcessInterleavedNoIndexIssue_ (procGit ["branch", "--remote"])
      & fmapRT (T.lines >>> fmap T.strip)
  when
    (("origin/" <> branchPrefix <> pName) `elem` remoteBranches)
    (throwE "Update branch already on origin.")

inNixpkgsRepo :: IO Bool
inNixpkgsRepo = do
  currentDir <- getCurrentDirectory
  doesFileExist (currentDir <> "/nixos/release.nix")

commit :: MonadIO m => Text -> ExceptT Text m ()
commit ref =
  runProcessNoIndexIssue_ (procGit ["commit", "-am", T.unpack ref])

headHash :: MonadIO m => ExceptT Text m Text
headHash = T.strip <$> readProcessInterleavedNoIndexIssue_ (procGit ["rev-parse", "HEAD"])

deleteBranchesEverywhere :: Vector Text -> IO ()
deleteBranchesEverywhere branches = do
  let branchList = V.toList $ branches
  result <- runExceptT $ runProcessNoIndexIssue_ (delete branchList)
  case result of
    Left error1 -> T.putStrLn $ tshow error1
    Right success1 -> T.putStrLn $ tshow success1
  result2 <- runExceptT $ runProcessNoIndexIssue_ (deleteOrigin branchList)
  case result2 of
    Left error2 -> T.putStrLn $ tshow error2
    Right success2 -> T.putStrLn $ tshow success2

runProcessNoIndexIssue_ ::
  MonadIO m => ProcessConfig () () () -> ExceptT Text m ()
runProcessNoIndexIssue_ config = tryIOTextET go
  where
    go = do
      (code, out, e) <- readProcess config
      case code of
        ExitFailure 128
          | "index.lock" `BS.isInfixOf` BSL.toStrict e -> do
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
          | "index.lock" `BS.isInfixOf` BSL.toStrict out -> do
            threadDelay 100000
            go
        ExitSuccess -> return $ bytestringToText out
        ExitFailure _ -> throw $ ExitCodeException code config out out
