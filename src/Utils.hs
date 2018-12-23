{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Utils
  ( Options(..)
  , Version
  , UpdateEnv(..)
  , canFail
  , orElse
  , setupNixpkgs
  , tRead
  , parseUpdates
  , succeded
  , shE
  , shRE
  , shellyET
  , overwriteErrorT
  , rewriteError
  , eitherToError
  , branchName
  , ourShell
  , ourSilentShell
  ) where

import Control.Category ((>>>))
import Control.Error
import Control.Exception (Exception)
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (FilePath)
import Shelly.Lifted
import System.Directory
import System.Environment
import System.Environment.XDG.BaseDir

default (T.Text)

type Version = Text

data Options = Options
  { dryRun :: Bool
  , workingDir :: Text
  , githubToken :: Text
  } deriving (Show)

data UpdateEnv = UpdateEnv
  { packageName :: Text
  , oldVersion :: Version
  , newVersion :: Version
  , options :: Options
  }

setupNixpkgs :: IO ()
setupNixpkgs = do
  fp <- getUserCacheDir "nixpkgs"
  exists <- doesDirectoryExist fp
  unless exists $ do
    shelly $ run "hub" ["clone", "nixpkgs", T.pack fp] -- requires that user has forked nixpkgs
    setCurrentDirectory fp
    shelly $
      cmd "git" "remote" "add" "upstream" "https://github.com/NixOS/nixpkgs"
    shelly $ cmd "git" "fetch" "upstream"
  setCurrentDirectory fp
  setEnv "NIX_PATH" ("nixpkgs=" <> fp)

-- | Set environment variables needed by various programs
setUpEnvironment :: Options -> Sh ()
setUpEnvironment options = do
  setenv "PAGER" ""
  setenv "GITHUB_TOKEN" (githubToken options)

ourSilentShell :: Options -> Sh a -> IO a
ourSilentShell o s =
  shelly $
  silently $ do
    setUpEnvironment o
    s

ourShell :: Options -> Sh a -> IO a
ourShell o s =
  shelly $
  verbosely $ do
    setUpEnvironment o
    s

shE :: Sh a -> Sh (Either Text a)
shE s = do
  r <- canFail s
  status <- lastExitCode
  case status of
    0 -> return $ Right r
    c -> return $ Left ("Exit code: " <> T.pack (show c))

-- A shell cmd we are expecting to fail and want to look at the output
-- of it.
shRE :: Sh a -> Sh (Either Text Text)
shRE s = do
  canFail s
  stderr <- lastStderr
  status <- lastExitCode
  case status of
    0 -> return $ Left ""
    c -> return $ Right stderr

shellyET :: MonadIO m => Sh a -> ExceptT Text m a
shellyET = shE >>> shelly >>> ExceptT

overwriteErrorT :: MonadIO m => Text -> ExceptT Text m a -> ExceptT Text m a
overwriteErrorT t = fmapLT (const t)

rewriteError :: Text -> Sh (Either Text a) -> Sh (Either Text a)
rewriteError t = fmap (first (const t))

eitherToError :: (Text -> Sh a) -> Sh (Either Text a) -> Sh a
eitherToError errorExit s = do
  e <- s
  either errorExit return e

canFail :: Sh a -> Sh a
canFail = errExit False

succeded :: Sh a -> Sh Bool
succeded s = do
  canFail s
  status <- lastExitCode
  return (status == 0)

orElse :: Sh a -> Sh a -> Sh a
orElse a b = do
  v <- canFail a
  status <- lastExitCode
  if status == 0
    then return v
    else b

infixl 3 `orElse`

branchName :: UpdateEnv -> Text
branchName ue = "auto-update/" <> packageName ue

parseUpdates :: Text -> [Either Text (Text, Version, Version)]
parseUpdates = map (toTriple . T.words) . T.lines
  where
    toTriple :: [Text] -> Either Text (Text, Version, Version)
    toTriple [package, oldVersion, newVersion] =
      Right (package, oldVersion, newVersion)
    toTriple line = Left $ "Unable to parse update: " <> T.unwords line

tRead :: Read a => Text -> a
tRead = read . T.unpack
