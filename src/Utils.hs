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
  ) where

import OurPrelude

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
    _ <- shelly $ run "hub" ["clone", "nixpkgs", T.pack fp] -- requires that user has forked nixpkgs
    setCurrentDirectory fp
    _ <-
      shelly $
      cmd "git" "remote" "add" "upstream" "https://github.com/NixOS/nixpkgs"
    shelly $ cmd "git" "fetch" "upstream"
  setCurrentDirectory fp
  setEnv "NIX_PATH" ("nixpkgs=" <> fp)

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
