{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Utils
  ( Options(..)
  , Version
  , UpdateEnv(..)
  , setupNixpkgs
  , tRead
  , parseUpdates
  , overwriteErrorT
  , branchName
  , runtimeDir
  , srcOrMain
  ) where

import OurPrelude

import Data.Bits ((.|.))
import qualified Data.Text as T
import Shelly.Lifted hiding (FilePath)
import System.Directory (doesDirectoryExist, setCurrentDirectory)
import System.Environment.XDG.BaseDir
import System.Posix.Directory (createDirectory)
import System.Posix.Env (getEnv, setEnv)
import System.Posix.Files
  ( directoryMode
  , fileExist
  , groupModes
  , otherExecuteMode
  , otherReadMode
  , ownerModes
  )
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (FileMode)

default (T.Text)

type Version = Text

data Options = Options
  { dryRun :: Bool
  , githubToken :: Text
  } deriving (Show)

data UpdateEnv = UpdateEnv
  { packageName :: Text
  , oldVersion :: Version
  , newVersion :: Version
  , options :: Options
  }

regDirMode :: FileMode
regDirMode =
  directoryMode .|. ownerModes .|. groupModes .|. otherReadMode .|.
  otherExecuteMode

xdgRuntimeDir :: MonadIO m => ExceptT Text m FilePath
xdgRuntimeDir = do
  xDir <-
    noteT "Could not get environment variable XDG_RUNTIME_DIR" $
    MaybeT $ liftIO $ getEnv "XDG_RUNTIME_DIR"
  xDirExists <- liftIO $ doesDirectoryExist xDir
  tryAssert ("XDG_RUNTIME_DIR " <> T.pack xDir <> " does not exist.") xDirExists
  let dir = xDir <> "/nixpkgs-update"
  dirExists <- liftIO $ fileExist dir
  unless
    dirExists
    (liftIO $
     putStrLn "creating xdgRuntimeDir" >> createDirectory dir regDirMode)
  return dir

tmpRuntimeDir :: MonadIO m => ExceptT Text m FilePath
tmpRuntimeDir = do
  dir <- liftIO $ mkdtemp "nixpkgs-update"
  dirExists <- liftIO $ doesDirectoryExist dir
  tryAssert
    ("Temporary directory " <> T.pack dir <> " does not exist.")
    dirExists
  return dir

runtimeDir :: IO FilePath
runtimeDir = do
  r <-
    runExceptT
      (xdgRuntimeDir <|> tmpRuntimeDir <|>
       throwE
         "Failed to create runtimeDir from XDG_RUNTIME_DIR or tmp directory")
  case r of
    Right dir -> return dir
    Left e -> error $ T.unpack e

setupNixpkgs :: Options -> IO ()
setupNixpkgs o = do
  fp <- getUserCacheDir "nixpkgs"
  exists <- doesDirectoryExist fp
  unless exists $ do
    _ <-
      shelly $ do
        setenv "GITHUB_TOKEN" (githubToken o)
        run "hub" ["clone", "nixpkgs", T.pack fp] -- requires that user has forked nixpkgs
    setCurrentDirectory fp
    _ <-
      shelly $
      cmd "git" "remote" "add" "upstream" "https://github.com/NixOS/nixpkgs"
    shelly $ cmd "git" "fetch" "upstream"
  setCurrentDirectory fp
  setEnv "NIX_PATH" ("nixpkgs=" <> fp) True

overwriteErrorT :: MonadIO m => Text -> ExceptT Text m a -> ExceptT Text m a
overwriteErrorT t = fmapLT (const t)

branchName :: UpdateEnv -> Text
branchName ue = "auto-update/" <> packageName ue

parseUpdates :: Text -> [Either Text (Text, Version, Version)]
parseUpdates = map (toTriple . T.words) . T.lines
  where
    toTriple :: [Text] -> Either Text (Text, Version, Version)
    toTriple [package, oldVer, newVer] = Right (package, oldVer, newVer)
    toTriple line = Left $ "Unable to parse update: " <> T.unwords line

tRead :: Read a => Text -> a
tRead = read . T.unpack

srcOrMain :: MonadIO m => (Text -> ExceptT Text m a) -> Text -> ExceptT Text m a
srcOrMain et attrPath = et (attrPath <> ".src") <|> et attrPath
