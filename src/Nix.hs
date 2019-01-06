{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Nix
  ( nixEvalET
  , assertNewerVersion
  , lookupAttrPath
  , getDerivationFile
  , getMaintainers
  , getOldHash
  , getSrcUrl
  , getSrcUrls
  , getIsBroken
  , build
  , getDescription
  , cachix
  , assertOneOrFewerFetcher
  , getHashFromBuild
  , assertOldVersionOn
  , resultLink
  , sha256Zero
  ) where

import OurPrelude

import Control.Monad (void)
import qualified Data.Text as T
import Prelude hiding (FilePath)
import qualified Shell
import Shelly (FilePath, Sh, cmd, fromText, run, setStdin, shelly, toTextIgnore)
import Utils (UpdateEnv(..), overwriteErrorT)

data Raw
  = Raw
  | NoRaw

rawOpt :: Raw -> [Text]
rawOpt Raw = ["--raw"]
rawOpt NoRaw = []

nixEvalET :: MonadIO m => Raw -> Text -> ExceptT Text m Text
nixEvalET raw expr =
  run "nix" (["eval", "-f", "."] <> rawOpt raw <> [expr]) & fmap T.strip &
  Shell.shellyET &
  overwriteErrorT ("nix eval failed for " <> expr)

-- Error if the "new version" is actually newer according to nix
assertNewerVersion :: MonadIO m => UpdateEnv -> ExceptT Text m ()
assertNewerVersion updateEnv = do
  versionComparison <-
    nixEvalET
      NoRaw
      ("(builtins.compareVersions \"" <> newVersion updateEnv <> "\" \"" <>
       oldVersion updateEnv <>
       "\")")
  case versionComparison of
    "1" -> return ()
    a ->
      throwE
        (newVersion updateEnv <> " is not newer than " <> oldVersion updateEnv <>
         " according to Nix; versionComparison: " <>
         a)

-- This is extremely slow but gives us the best results we know of
lookupAttrPath :: MonadIO m => UpdateEnv -> ExceptT Text m Text
lookupAttrPath updateEnv =
  cmd
    "nix-env"
    "-qa"
    (packageName updateEnv <> "-" <> oldVersion updateEnv)
    "-f"
    "."
    "--attr-path"
    "--arg"
    "config"
    "{ allowBroken = true; allowUnfree = true; allowAliases = false; }" &
  fmap (T.lines >>> head >>> T.words >>> head) &
  Shell.shellyET &
  overwriteErrorT "nix-env -q failed to find package name with old version"

getDerivationFile :: MonadIO m => Text -> ExceptT Text m FilePath
getDerivationFile attrPath =
  cmd "env" "EDITOR=echo" "nix" "edit" attrPath "-f" "." & fmap T.strip &
  fmap fromText &
  Shell.shellyET &
  overwriteErrorT "Couldn't find derivation file."

getHash :: MonadIO m => Text -> ExceptT Text m Text
getHash attrPath =
  nixEvalET Raw ("pkgs." <> attrPath <> ".src.drvAttrs.outputHash") <|>
  nixEvalET Raw ("pkgs." <> attrPath <> ".drvAttrs.outputHash")

getOldHash :: MonadIO m => Text -> ExceptT Text m Text
getOldHash attrPath =
  getHash attrPath &
  overwriteErrorT
    ("Could not find old output hash at " <> attrPath <>
     ".src.drvAttrs.outputHash or .drvAttrs.outputHash.")

getMaintainers :: MonadIO m => Text -> ExceptT Text m Text
getMaintainers attrPath =
  nixEvalET
    Raw
    ("(let pkgs = import ./. {}; gh = m : m.github or \"\"; nonempty = s: s != \"\"; addAt = s: \"@\"+s; in builtins.concatStringsSep \" \" (map addAt (builtins.filter nonempty (map gh pkgs." <>
     attrPath <>
     ".meta.maintainers or []))))") &
  overwriteErrorT ("Could not fetch maintainers for" <> attrPath)

readNixBool :: MonadIO m => ExceptT Text m Text -> ExceptT Text m Bool
readNixBool t = do
  text <- t
  case text of
    "true" -> return True
    "false" -> return False
    a -> throwE ("Failed to read expected nix boolean " <> a)

getIsBroken :: MonadIO m => Text -> ExceptT Text m Bool
getIsBroken attrPath =
  nixEvalET
    NoRaw
    ("(let pkgs = import ./. {}; in pkgs." <> attrPath <>
     ".meta.broken or false)") &
  readNixBool &
  overwriteErrorT ("Could not get meta.broken for attrpath " <> attrPath)

getDescription :: MonadIO m => Text -> ExceptT Text m Text
getDescription attrPath =
  nixEvalET
    NoRaw
    ("(let pkgs = import ./. {}; in pkgs." <> attrPath <>
     ".meta.description or \"\")") &
  overwriteErrorT ("Could not get meta.description for attrpath " <> attrPath)

getSrcUrl :: MonadIO m => Text -> ExceptT Text m Text
getSrcUrl attrPath =
  nixEvalET
    Raw
    ("(let pkgs = import ./. {}; in builtins.elemAt pkgs." <> attrPath <>
     ".src.drvAttrs.urls 0)") <|>
  nixEvalET
    Raw
    ("(let pkgs = import ./. {}; in builtins.elemAt pkgs." <> attrPath <>
     ".drvAttrs.urls 0)")

getSrcAttr :: MonadIO m => Text -> Text -> ExceptT Text m Text
getSrcAttr attr attrPath =
  nixEvalET NoRaw ("pkgs." <> attrPath <> ".src." <> attr) <|>
  nixEvalET NoRaw ("pkgs." <> attrPath <> "." <> attr)

getSrcUrls :: MonadIO m => Text -> ExceptT Text m Text
getSrcUrls = getSrcAttr "urls"

buildCmd :: Text -> Sh ()
buildCmd =
  cmd
    "nix-build"
    "--option"
    "sandbox"
    "true"
    "--option"
    "restrict-eval"
    "true"
    "-A"

build :: MonadIO m => Text -> ExceptT Text m ()
build attrPath =
  (buildCmd attrPath & Shell.shellyET) <|>
  (do _ <- buildFailedLog
      throwE "nix log failed trying to get build logs")
  where
    buildFailedLog = do
      buildLog <-
        cmd "nix" "log" "-f" "." attrPath & Shell.shellyET &
        fmap (T.lines >>> reverse >>> take 30 >>> reverse >>> T.unlines)
      throwE ("nix build failed.\n" <> buildLog)

cachix :: MonadIO m => FilePath -> m ()
cachix resultPath =
  shelly $ do
    setStdin (toTextIgnore resultPath)
    void $ Shell.shE $ cmd "cachix" "push" "r-ryantm"

numberOfFetchers :: Text -> Int
numberOfFetchers derivationContents =
  count "fetchurl {" + count "fetchgit {" + count "fetchFromGitHub {"
  where
    count x = T.count x derivationContents

assertOneOrFewerFetcher :: MonadIO m => Text -> FilePath -> ExceptT Text m ()
assertOneOrFewerFetcher derivationContents derivationFile =
  tryAssert
    ("More than one fetcher in " <> toTextIgnore derivationFile)
    (numberOfFetchers derivationContents <= 1)

assertOldVersionOn ::
     MonadIO m => UpdateEnv -> Text -> Text -> ExceptT Text m ()
assertOldVersionOn updateEnv branchName contents =
  tryAssert
    ("Old version not present in " <> branchName <> " derivation file.")
    (oldVersionPattern `T.isInfixOf` contents)
  where
    oldVersionPattern = "\"" <> oldVersion updateEnv <> "\""

resultLink :: MonadIO m => ExceptT Text m FilePath
resultLink =
  (T.strip >>> fromText) <$> do
    Shell.shellyET (cmd "readlink" "./result") <|>
      Shell.shellyET (cmd "readlink" "./result-bin") <|>
      throwE "Could not find result link."

sha256Zero :: Text
sha256Zero = "0000000000000000000000000000000000000000000000000000"

-- fixed-output derivation produced path '/nix/store/fg2hz90z5bc773gpsx4gfxn3l6fl66nw-source' with sha256 hash '0q1lsgc1621czrg49nmabq6am9sgxa9syxrwzlksqqr4dyzw4nmf' instead of the expected hash '0bp22mzkjy48gncj5vm9b7whzrggcbs5pd4cnb6k8jpl9j02dhdv'
getHashFromBuild :: Text -> ExceptT Text Sh Text
getHashFromBuild attrPath = do
  stderr <-
    (ExceptT $ Shell.shRE (buildCmd attrPath)) <|>
    throwE "Build succeeded unexpectedly"
  let firstSplit = T.splitOn "with sha256 hash '" stderr
  firstSplitSecondPart <- tryLast "stdout did not split as expected" firstSplit
  let secondSplit =
        T.splitOn
          "' instead of the expected hash '0000000000000000000000000000000000000000000000000000'"
          firstSplitSecondPart
  tryHead "stdout did not split second part as expected" secondSplit
