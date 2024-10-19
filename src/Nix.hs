{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Nix
  ( assertNewerVersion,
    assertOldVersionOn,
    binPath,
    build,
    cachix,
    getAttr,
    getAttrString,
    getChangelog,
    getDerivationFile,
    getDescription,
    getHash,
    getHashFromBuild,
    getHomepage,
    getIsBroken,
    getMaintainers,
    getPatches,
    getSrcUrl,
    hasPatchNamed,
    hasUpdateScript,
    lookupAttrPath,
    numberOfFetchers,
    numberOfHashes,
    resultLink,
    runUpdateScript,
    fakeHashMatching,
    version,
    Raw (..),
  )
where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Git
import Language.Haskell.TH.Env (envQ)
import OurPrelude
import System.Exit ()
import qualified System.Process.Typed as TP
import Utils (UpdateEnv (..), nixBuildOptions, nixCommonOptions, srcOrMain)
import Prelude hiding (log)

binPath :: String
binPath = fromJust ($$(envQ "NIX") :: Maybe String) <> "/bin"

data Env = Env [(String, String)]

data Raw
  = Raw
  | NoRaw

data EvalOptions = EvalOptions Raw Env

rawOpt :: Raw -> [String]
rawOpt Raw = ["--raw"]
rawOpt NoRaw = []

nixEvalApply ::
  (MonadIO m) =>
  Text ->
  Text ->
  ExceptT Text m Text
nixEvalApply applyFunc attrPath =
  ourReadProcess_
    (proc (binPath <> "/nix") (["--extra-experimental-features", "nix-command", "--extra-experimental-features", "flakes", "eval", ".#" <> T.unpack attrPath, "--apply", T.unpack applyFunc]))
    & fmapRT (fst >>> T.strip)

nixEvalApplyRaw ::
  (MonadIO m) =>
  Text ->
  Text ->
  ExceptT Text m Text
nixEvalApplyRaw applyFunc attrPath =
  ourReadProcess_
    (proc (binPath <> "/nix") (["--extra-experimental-features", "nix-command", "--extra-experimental-features", "flakes", "eval", ".#" <> T.unpack attrPath, "--raw", "--apply", T.unpack applyFunc]))
    & fmapRT (fst >>> T.strip)

nixEvalExpr ::
  (MonadIO m) =>
  Text ->
  ExceptT Text m Text
nixEvalExpr expr =
  ourReadProcess_
    (proc (binPath <> "/nix") (["--extra-experimental-features", "nix-command", "eval", "--expr", T.unpack expr]))
    & fmapRT (fst >>> T.strip)

-- Error if the "new version" is actually newer according to nix
assertNewerVersion :: (MonadIO m) => UpdateEnv -> ExceptT Text m ()
assertNewerVersion updateEnv = do
  versionComparison <-
    nixEvalExpr
      ( "(builtins.compareVersions \""
          <> newVersion updateEnv
          <> "\" \""
          <> oldVersion updateEnv
          <> "\")"
      )
  case versionComparison of
    "1" -> return ()
    a ->
      throwE
        ( newVersion updateEnv
            <> " is not newer than "
            <> oldVersion updateEnv
            <> " according to Nix; versionComparison: "
            <> a
            <> " "
        )

-- This is extremely slow but gives us the best results we know of
lookupAttrPath :: (MonadIO m) => UpdateEnv -> ExceptT Text m Text
lookupAttrPath updateEnv =
  -- lookup attrpath by nix-env
  ( proc
      (binPath <> "/nix-env")
      ( [ "-qa",
          (packageName updateEnv <> "-" <> oldVersion updateEnv) & T.unpack,
          "-f",
          ".",
          "--attr-path"
        ]
          <> nixCommonOptions
      )
      & ourReadProcess_
      & fmapRT (fst >>> T.lines >>> head >>> T.words >>> head)
  )
    <|>
    -- if that fails, check by attrpath
    (getAttrString "name" (packageName updateEnv))
    & fmapRT (const (packageName updateEnv))

getDerivationFile :: (MonadIO m) => Text -> ExceptT Text m Text
getDerivationFile attrPath = do
  npDir <- liftIO $ Git.nixpkgsDir
  proc "env" ["EDITOR=echo", (binPath <> "/nix"), "--extra-experimental-features", "nix-command", "edit", attrPath & T.unpack, "-f", "."]
    & ourReadProcess_
    & fmapRT (fst >>> T.strip >>> T.stripPrefix (T.pack npDir <> "/") >>> fromJust)

-- Get an attribute that can be evaluated off a derivation, as in:
-- getAttr "cargoSha256" "ripgrep" -> 0lwz661rbm7kwkd6mallxym1pz8ynda5f03ynjfd16vrazy2dj21
getAttr :: (MonadIO m) => Text -> Text -> ExceptT Text m Text
getAttr attr = srcOrMain (nixEvalApply ("p: p." <> attr))

getAttrString :: (MonadIO m) => Text -> Text -> ExceptT Text m Text
getAttrString attr = srcOrMain (nixEvalApplyRaw ("p: p." <> attr))

getHash :: (MonadIO m) => Text -> ExceptT Text m Text
getHash = getAttrString "drvAttrs.outputHash"

getMaintainers :: (MonadIO m) => Text -> ExceptT Text m Text
getMaintainers =
  nixEvalApplyRaw "p: let gh = m : m.github or \"\"; nonempty = s: s != \"\"; addAt = s: \"@\"+s; in builtins.concatStringsSep \" \" (map addAt (builtins.filter nonempty (map gh p.meta.maintainers or [])))"

readNixBool :: (MonadIO m) => ExceptT Text m Text -> ExceptT Text m Bool
readNixBool t = do
  text <- t
  case text of
    "true" -> return True
    "false" -> return False
    a -> throwE ("Failed to read expected nix boolean " <> a <> " ")

getIsBroken :: (MonadIO m) => Text -> ExceptT Text m Bool
getIsBroken attrPath =
  getAttr "meta.broken" attrPath
    & readNixBool

getChangelog :: (MonadIO m) => Text -> ExceptT Text m Text
getChangelog = nixEvalApplyRaw "p: p.meta.changelog or \"\""

getDescription :: (MonadIO m) => Text -> ExceptT Text m Text
getDescription = nixEvalApplyRaw "p: p.meta.description or \"\""

getHomepage :: (MonadIO m) => Text -> ExceptT Text m Text
getHomepage = nixEvalApplyRaw "p: p.meta.homepage or \"\""

getSrcUrl :: (MonadIO m) => Text -> ExceptT Text m Text
getSrcUrl =
  srcOrMain
    (nixEvalApplyRaw "p: builtins.elemAt p.drvAttrs.urls 0")

buildCmd :: Text -> ProcessConfig () () ()
buildCmd attrPath =
  silently $ proc (binPath <> "/nix-build") (nixBuildOptions ++ ["-A", attrPath & T.unpack])

log :: Text -> ProcessConfig () () ()
log attrPath = proc (binPath <> "/nix") ["--extra-experimental-features", "nix-command", "log", "-f", ".", attrPath & T.unpack]

build :: (MonadIO m) => Text -> ExceptT Text m ()
build attrPath =
  (buildCmd attrPath & runProcess_ & tryIOTextET)
    <|> ( do
            _ <- buildFailedLog
            throwE "nix log failed trying to get build logs "
        )
  where
    buildFailedLog = do
      buildLog <-
        ourReadProcessInterleaved_ (log attrPath)
          & fmap (T.lines >>> reverse >>> take 30 >>> reverse >>> T.unlines)
      throwE ("nix build failed.\n" <> buildLog <> " ")

cachix :: (MonadIO m) => Text -> ExceptT Text m ()
cachix resultPath =
  ( setStdin
      (byteStringInput (TL.encodeUtf8 (TL.fromStrict resultPath)))
      (shell "cachix push nix-community")
      & runProcess_
      & tryIOTextET
  )
    <|> throwE "pushing to cachix failed"

numberOfFetchers :: Text -> Int
numberOfFetchers derivationContents =
  countUp "fetchurl {" + countUp "fetchgit {" + countUp "fetchFromGitHub {"
  where
    countUp x = T.count x derivationContents

-- Sum the number of things that look like fixed-output derivation hashes
numberOfHashes :: Text -> Int
numberOfHashes derivationContents =
  sum $ map countUp ["sha256 =", "sha256=", "cargoSha256 =", "cargoHash =", "vendorSha256 =", "vendorHash =", "hash =", "npmDepsHash ="]
  where
    countUp x = T.count x derivationContents

assertOldVersionOn ::
  (MonadIO m) => UpdateEnv -> Text -> Text -> ExceptT Text m ()
assertOldVersionOn updateEnv branchName contents =
  tryAssert
    ("Old version " <> oldVersionPattern <> " not present in " <> branchName <> " derivation file with contents: " <> contents)
    (oldVersionPattern `T.isInfixOf` contents)
  where
    oldVersionPattern = oldVersion updateEnv <> "\""

resultLink :: (MonadIO m) => ExceptT Text m Text
resultLink =
  T.strip
    <$> ( ourReadProcessInterleaved_ "readlink ./result"
            <|> ourReadProcessInterleaved_ "readlink ./result-bin"
            <|> ourReadProcessInterleaved_ "readlink ./result-dev"
        )
    <|> throwE "Could not find result link. "

fakeHashMatching :: Text -> Text
fakeHashMatching oldHash =
  if "sha512-" `T.isPrefixOf` oldHash
    then "sha512-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=="
    else "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="

-- fixed-output derivation produced path '/nix/store/fg2hz90z5bc773gpsx4gfxn3l6fl66nw-source' with sha256 hash '0q1lsgc1621czrg49nmabq6am9sgxa9syxrwzlksqqr4dyzw4nmf' instead of the expected hash '0bp22mzkjy48gncj5vm9b7whzrggcbs5pd4cnb6k8jpl9j02dhdv'
getHashFromBuild :: (MonadIO m) => Text -> ExceptT Text m Text
getHashFromBuild =
  srcOrMain
    ( \attrPath -> do
        (exitCode, _, stderr) <- buildCmd attrPath & readProcess
        when (exitCode == ExitSuccess) $ throwE "build succeeded unexpectedly"
        let stdErrText = bytestringToText stderr
        let firstSplit = T.splitOn "got:    " stdErrText
        firstSplitSecondPart <-
          tryAt
            ("stderr did not split as expected full stderr was: \n" <> stdErrText)
            firstSplit
            1
        let secondSplit = T.splitOn "\n" firstSplitSecondPart
        tryHead
          ( "stderr did not split second part as expected full stderr was: \n"
              <> stdErrText
              <> "\nfirstSplitSecondPart:\n"
              <> firstSplitSecondPart
          )
          secondSplit
    )

version :: (MonadIO m) => ExceptT Text m Text
version = ourReadProcessInterleaved_ (proc (binPath <> "/nix") ["--version"])

getPatches :: (MonadIO m) => Text -> ExceptT Text m Text
getPatches =
  nixEvalApply "p: map (patch: patch.name) p.patches"

hasPatchNamed :: (MonadIO m) => Text -> Text -> ExceptT Text m Bool
hasPatchNamed attrPath name = do
  ps <- getPatches attrPath
  return $ name `T.isInfixOf` ps

hasUpdateScript :: (MonadIO m) => Text -> ExceptT Text m Bool
hasUpdateScript attrPath = do
  nixEvalApply
    "p: builtins.hasAttr \"updateScript\" p"
    attrPath
    & readNixBool

runUpdateScript :: (MonadIO m) => Text -> ExceptT Text m (ExitCode, Text)
runUpdateScript attrPath = do
  let timeout = "10m" :: Text
  (exitCode, output) <-
    ourReadProcessInterleaved $
      TP.setStdin (TP.byteStringInput "\n") $
        proc "timeout" [T.unpack timeout, "nix-shell", "maintainers/scripts/update.nix", "--argstr", "package", T.unpack attrPath]
  case exitCode of
    ExitFailure 124 -> do
      return (exitCode, "updateScript for " <> attrPath <> " took longer than " <> timeout <> " and timed out. Other output: " <> output)
    _ -> do
      return (exitCode, output)
