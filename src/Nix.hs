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
  , getOutpaths
  , parseStringList
  , build
  , getDescription
  , getHomepage
  , cachix
  , assertOneOrFewerFetcher
  , getHashFromBuild
  , assertOldVersionOn
  , resultLink
  , sha256Zero
  ) where

import OurPrelude
import Prelude hiding (log)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector as V
import System.Exit
import System.Process.Typed
import Text.Parsec (parse)
import Text.Parser.Combinators
import Text.Parser.Token
import Utils (UpdateEnv(..), overwriteErrorT, srcOrMain)

data Raw
  = Raw
  | NoRaw

rawOpt :: Raw -> [String]
rawOpt Raw = ["--raw"]
rawOpt NoRaw = []

nixEvalET :: MonadIO m => Raw -> Text -> ExceptT Text m Text
nixEvalET raw expr =
  ourReadProcessInterleaved_
    (proc "nix" (["eval", "-f", "."] <> rawOpt raw <> [T.unpack expr])) &
  fmapRT T.strip &
  overwriteErrorT ("nix eval failed for " <> expr <> " ")

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
         a <>
         " ")

-- This is extremely slow but gives us the best results we know of
lookupAttrPath :: MonadIO m => UpdateEnv -> ExceptT Text m Text
lookupAttrPath updateEnv =
  proc
    "nix-env"
    [ "-qa"
    , (packageName updateEnv <> "-" <> oldVersion updateEnv) & T.unpack
    , "-f"
    , "."
    , "--attr-path"
    , "--arg"
    , "config"
    , "{ allowBroken = true; allowUnfree = true; allowAliases = false; }"
    ] &
  ourReadProcessInterleaved_ &
  fmapRT (T.lines >>> head >>> T.words >>> head) &
  overwriteErrorT "nix-env -q failed to find package name with old version "

getDerivationFile :: MonadIO m => Text -> ExceptT Text m FilePath
getDerivationFile attrPath =
  proc "env" ["EDITOR=echo", "nix", "edit", attrPath & T.unpack, "-f", "."] &
  ourReadProcessInterleaved_ &
  fmapRT (T.strip >>> T.unpack) &
  overwriteErrorT "Couldn't find derivation file. "

getHash :: MonadIO m => Text -> ExceptT Text m Text
getHash =
  srcOrMain
    (\attrPath -> nixEvalET Raw ("pkgs." <> attrPath <> ".drvAttrs.outputHash"))

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

parseStringList :: MonadIO m => Text -> ExceptT Text m (Vector Text)
parseStringList list =
  parse nixStringList ("nix list " ++ T.unpack list) list & fmapL tshow &
  hoistEither

nixStringList :: TokenParsing m => m (Vector Text)
nixStringList = V.fromList <$> brackets (many stringLiteral)

getOutpaths :: MonadIO m => Text -> ExceptT Text m (Vector Text)
getOutpaths attrPath = do
  list <- nixEvalET NoRaw (attrPath <> ".outputs")
  outputs <- parseStringList list
  V.sequence $ fmap (\o -> nixEvalET Raw (attrPath <> "." <> o)) outputs

readNixBool :: MonadIO m => ExceptT Text m Text -> ExceptT Text m Bool
readNixBool t = do
  text <- t
  case text of
    "true" -> return True
    "false" -> return False
    a -> throwE ("Failed to read expected nix boolean " <> a <> " ")

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

getHomepage :: MonadIO m => Text -> ExceptT Text m Text
getHomepage attrPath =
  nixEvalET
    NoRaw
    ("(let pkgs = import ./. {}; in pkgs." <> attrPath <>
     ".meta.homepage or \"\")") &
  overwriteErrorT ("Could not get meta.homepage for attrpath " <> attrPath)

getSrcUrl :: MonadIO m => Text -> ExceptT Text m Text
getSrcUrl =
  srcOrMain
    (\attrPath ->
       nixEvalET
         Raw
         ("(let pkgs = import ./. {}; in builtins.elemAt pkgs." <> attrPath <>
          ".drvAttrs.urls 0)"))

getSrcAttr :: MonadIO m => Text -> Text -> ExceptT Text m Text
getSrcAttr attr =
  srcOrMain (\attrPath -> nixEvalET NoRaw ("pkgs." <> attrPath <> "." <> attr))

getSrcUrls :: MonadIO m => Text -> ExceptT Text m Text
getSrcUrls = getSrcAttr "urls"

buildCmd :: Text -> ProcessConfig () () ()
buildCmd attrPath =
  silently $
  proc
    "nix-build"
    [ "--option"
    , "sandbox"
    , "true"
    , "--option"
    , "restrict-eval"
    , "true"
    , "--arg"
    , "config"
    , "{ allowBroken = true; allowUnfree = true; allowAliases = false; }"
    , "-A"
    , attrPath & T.unpack
    ]

log :: Text -> ProcessConfig () () ()
log attrPath = proc "nix" ["log", "-f", ".", attrPath & T.unpack]

build :: MonadIO m => Text -> ExceptT Text m ()
build attrPath =
  (buildCmd attrPath & runProcess_ & tryIOTextET) <|>
  (do _ <- buildFailedLog
      throwE "nix log failed trying to get build logs ")
  where
    buildFailedLog = do
      buildLog <-
        ourReadProcessInterleaved_ (log attrPath) &
        fmap (T.lines >>> reverse >>> take 30 >>> reverse >>> T.unlines)
      throwE ("nix build failed.\n" <> buildLog <> " ")

cachix :: MonadIO m => Text -> ExceptT Text m ()
cachix resultPath =
  (setStdin
     (byteStringInput (TL.encodeUtf8 (TL.fromStrict resultPath)))
     (shell "cachix push r-ryantm") &
   runProcess_ &
   tryIOTextET) <|>
  throwE "pushing to cachix failed"

numberOfFetchers :: Text -> Int
numberOfFetchers derivationContents =
  countUp "fetchurl {" + countUp "fetchgit {" + countUp "fetchFromGitHub {"
  where
    countUp x = T.count x derivationContents

assertOneOrFewerFetcher :: MonadIO m => Text -> FilePath -> ExceptT Text m ()
assertOneOrFewerFetcher derivationContents derivationFile =
  tryAssert
    ("More than one fetcher in " <> T.pack derivationFile)
    (numberOfFetchers derivationContents <= 1)

assertOldVersionOn ::
     MonadIO m => UpdateEnv -> Text -> Text -> ExceptT Text m ()
assertOldVersionOn updateEnv branchName contents =
  tryAssert
    ("Old version not present in " <> branchName <> " derivation file.")
    (oldVersionPattern `T.isInfixOf` contents)
  where
    oldVersionPattern = oldVersion updateEnv <> "\""

resultLink :: MonadIO m => ExceptT Text m Text
resultLink =
  T.strip <$> ourReadProcessInterleaved_ "readlink ./result" <|>
  ourReadProcessInterleaved_ "readlink ./result-bin" <|>
  throwE "Could not find result link. "

sha256Zero :: Text
sha256Zero = "0000000000000000000000000000000000000000000000000000"

-- fixed-output derivation produced path '/nix/store/fg2hz90z5bc773gpsx4gfxn3l6fl66nw-source' with sha256 hash '0q1lsgc1621czrg49nmabq6am9sgxa9syxrwzlksqqr4dyzw4nmf' instead of the expected hash '0bp22mzkjy48gncj5vm9b7whzrggcbs5pd4cnb6k8jpl9j02dhdv'
getHashFromBuild :: MonadIO m => Text -> ExceptT Text m Text
getHashFromBuild =
  srcOrMain
    (\attrPath -> do
       (exitCode, _, stderr) <- buildCmd attrPath & readProcess
       when (exitCode == ExitSuccess) $ throwE "build succeeded unexpectedly"
       let firstSplit = T.splitOn "with sha256 hash '" (bytestringToText stderr)
       firstSplitSecondPart <-
         tryAt "stderr did not split as expected " firstSplit 1
       let secondSplit =
             T.splitOn
               "' instead of the expected hash '0000000000000000000000000000000000000000000000000000'"
               firstSplitSecondPart
       tryHead "stderr did not split second part as expected " secondSplit)
