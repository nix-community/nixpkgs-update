{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Git
  ( findAutoUpdateBranchMessage,
    mergeBase,
    cleanAndResetTo,
    commit,
    deleteBranchesEverywhere,
    delete1,
    diff,
    diffFileNames,
    fetch,
    fetchIfStale,
    headRev,
    push,
    nixpkgsDir,
    setupNixpkgs,
    Git.show,
    worktreeAdd,
    worktreeRemove,
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
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getModificationTime, setCurrentDirectory)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Exit ()
import System.IO.Error (tryIOError)
import System.Posix.Env (setEnv)
import Utils (Options (..), UpdateEnv (..), branchName, branchPrefix)

bin :: String
bin = fromJust ($$(envQ "GIT") :: Maybe String) <> "/bin/git"

procGit :: [String] -> ProcessConfig () () ()
procGit = proc bin

clean :: ProcessConfig () () ()
clean = silently $ procGit ["clean", "-fdx"]

worktreeAdd :: FilePath -> Text -> UpdateEnv -> IO ()
worktreeAdd path commitish updateEnv =
  runProcessNoIndexIssue_IO $ silently $ procGit ["worktree", "add", "-b", T.unpack (branchName updateEnv), path, T.unpack commitish]

worktreeRemove :: FilePath -> IO ()
worktreeRemove path = do
  exist <- doesDirectoryExist path
  if exist
    then runProcessNoIndexIssue_IO $ silently $ procGit ["worktree", "remove", "--force", path]
    else return ()

checkout :: Text -> Text -> ProcessConfig () () ()
checkout branch target =
  silently $ procGit ["checkout", "-B", T.unpack branch, T.unpack target]

reset :: Text -> ProcessConfig () () ()
reset target = silently $ procGit ["reset", "--hard", T.unpack target]

delete1 :: Text -> IO ()
delete1 bName = ignoreExitCodeException $ runProcessNoIndexIssue_IO (delete1' bName)

delete1' :: Text -> ProcessConfig () () ()
delete1' branch = delete [branch]

delete :: [Text] -> ProcessConfig () () ()
delete branches = silently $ procGit (["branch", "-D"] ++ fmap T.unpack branches)

deleteOrigin :: [Text] -> ProcessConfig () () ()
deleteOrigin branches =
  silently $ procGit (["push", "origin", "--delete"] ++ fmap T.unpack branches)

cleanAndResetTo :: (MonadIO m) => Text -> ExceptT Text m ()
cleanAndResetTo branch =
  let target = "upstream/" <> branch
   in do
        runProcessNoIndexIssue_ $ silently $ procGit ["reset", "--hard"]
        runProcessNoIndexIssue_ clean
        runProcessNoIndexIssue_ $ checkout branch target
        runProcessNoIndexIssue_ $ reset target
        runProcessNoIndexIssue_ clean

show :: (MonadIO m) => Text -> Text -> ExceptT Text m Text
show branch file =
  readProcessInterleavedNoIndexIssue_ $ silently $ procGit ["show", T.unpack ("remotes/upstream/" <> branch <> ":" <> file)]

diff :: (MonadIO m) => Text -> ExceptT Text m Text
diff branch = readProcessInterleavedNoIndexIssue_ $ procGit ["diff", T.unpack branch]

diffFileNames :: (MonadIO m) => Text -> ExceptT Text m [Text]
diffFileNames branch =
  readProcessInterleavedNoIndexIssue_ (procGit ["diff", T.unpack branch, "--name-only"])
    & fmapRT T.lines

staleFetchHead :: (MonadIO m) => m Bool
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

fetchIfStale :: (MonadIO m) => ExceptT Text m ()
fetchIfStale = whenM staleFetchHead fetch

fetch :: (MonadIO m) => ExceptT Text m ()
fetch =
  runProcessNoIndexIssue_ $
    silently $
      procGit ["fetch", "-q", "--prune", "--multiple", "upstream", "origin"]

push :: (MonadIO m) => UpdateEnv -> ExceptT Text m ()
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
setupNixpkgs ghUser = do
  fp <- nixpkgsDir
  exists <- doesDirectoryExist fp
  unless exists $ do
    procGit ["clone", "--origin", "upstream", "https://github.com/NixOS/nixpkgs.git", fp]
      & runProcess_
    setCurrentDirectory fp
    procGit ["remote", "add", "origin", "https://github.com/" <> T.unpack ghUser <> "/nixpkgs.git"]
      -- requires that user has forked nixpkgs
      & runProcess_
  inNixpkgs <- inNixpkgsRepo
  unless inNixpkgs do
    setCurrentDirectory fp
    _ <- runExceptT fetchIfStale
    _ <- runExceptT $ cleanAndResetTo "master"
    return ()
  System.Posix.Env.setEnv "NIX_PATH" ("nixpkgs=" <> fp) True

mergeBase :: IO Text
mergeBase = do
  readProcessInterleavedNoIndexIssue_IO
    (procGit ["merge-base", "upstream/master", "upstream/staging"])
    & fmap T.strip

-- Return Nothing if a remote branch for this package doesn't exist. If a
-- branch does exist, return a Just of its last commit message.
findAutoUpdateBranchMessage :: (MonadIO m) => Text -> ExceptT Text m (Maybe Text)
findAutoUpdateBranchMessage pName = do
  remoteBranches <-
    readProcessInterleavedNoIndexIssue_ (procGit ["branch", "--remote", "--format=%(refname:short) %(subject)"])
      & fmapRT (T.lines >>> fmap (T.strip >>> T.breakOn " "))
  return $
    lookup ("origin/" <> branchPrefix <> pName) remoteBranches
      & fmap (T.drop 1)

inNixpkgsRepo :: IO Bool
inNixpkgsRepo = do
  currentDir <- getCurrentDirectory
  doesFileExist (currentDir <> "/nixos/release.nix")

commit :: (MonadIO m) => Text -> ExceptT Text m ()
commit ref =
  runProcessNoIndexIssue_ (procGit ["commit", "-am", T.unpack ref])

headRev :: (MonadIO m) => ExceptT Text m Text
headRev = T.strip <$> readProcessInterleavedNoIndexIssue_ (procGit ["rev-parse", "HEAD"])

deleteBranchesEverywhere :: Vector Text -> IO ()
deleteBranchesEverywhere branches = do
  let branchList = V.toList $ branches
  if null branchList
    then return ()
    else do
      result <- runExceptT $ runProcessNoIndexIssue_ (delete branchList)
      case result of
        Left error1 -> T.putStrLn $ tshow error1
        Right success1 -> T.putStrLn $ tshow success1
      result2 <- runExceptT $ runProcessNoIndexIssue_ (deleteOrigin branchList)
      case result2 of
        Left error2 -> T.putStrLn $ tshow error2
        Right success2 -> T.putStrLn $ tshow success2

runProcessNoIndexIssue_IO ::
  ProcessConfig () () () -> IO ()
runProcessNoIndexIssue_IO config = go
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

runProcessNoIndexIssue_ ::
  (MonadIO m) => ProcessConfig () () () -> ExceptT Text m ()
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
  (MonadIO m) => ProcessConfig () () () -> ExceptT Text m Text
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

readProcessInterleavedNoIndexIssue_IO ::
  ProcessConfig () () () -> IO Text
readProcessInterleavedNoIndexIssue_IO config = go
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
