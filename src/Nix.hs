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
    getChangelog,
    getDerivationFile,
    getDescription,
    getDrvAttr,
    getHash,
    getHashFromBuild,
    getHomepage,
    getHomepageET,
    getIsBroken,
    getMaintainers,
    getOldHash,
    getOutpaths,
    getPatches,
    getSrcUrl,
    getSrcUrls,
    hasPatchNamed,
    hasUpdateScript,
    lookupAttrPath,
    nixEvalET,
    numberOfFetchers,
    numberOfHashes,
    parseStringList,
    resultLink,
    runUpdateScript,
    sha256Zero,
    version,
    Raw (..),
  )
where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector as V
import Language.Haskell.TH.Env (envQ)
import OurPrelude
import qualified Polysemy.Error as Error
import qualified System.Process.Typed as TP
import qualified Process
import qualified Process as P
import System.Exit
import Text.Parsec (parse)
import Text.Parser.Combinators
import Text.Parser.Token
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

nixEvalSem ::
  Members '[P.Process, Error Text] r =>
  EvalOptions ->
  Text ->
  Sem r (Text, Text)
nixEvalSem (EvalOptions raw (Env env)) expr =
  (\(stdout, stderr) -> (T.strip stdout, T.strip stderr))
    <$> ourReadProcess_Sem
      (setEnv env (proc (binPath <> "/nix") (["eval", "-f", "."] <> rawOpt raw <> [T.unpack expr])))

nixEvalET :: MonadIO m => EvalOptions -> Text -> ExceptT Text m Text
nixEvalET (EvalOptions raw (Env env)) expr =
  ourReadProcess_
    (setEnv env (proc (binPath <> "/nix") (["eval", "-f", "."] <> rawOpt raw <> [T.unpack expr])))
    & fmapRT (fst >>> T.strip)

-- Error if the "new version" is actually newer according to nix
assertNewerVersion :: MonadIO m => UpdateEnv -> ExceptT Text m ()
assertNewerVersion updateEnv = do
  versionComparison <-
    nixEvalET
      (EvalOptions NoRaw (Env []))
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
lookupAttrPath :: MonadIO m => UpdateEnv -> ExceptT Text m Text
lookupAttrPath updateEnv =
  proc
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

getDerivationFile :: MonadIO m => Text -> ExceptT Text m FilePath
getDerivationFile attrPath =
  proc "env" ["EDITOR=echo", (binPath <> "/nix"), "edit", attrPath & T.unpack, "-f", "."]
    & ourReadProcess_
    & fmapRT (fst >>> T.strip >>> T.unpack)

getDrvAttr :: MonadIO m => Text -> Text -> ExceptT Text m Text
getDrvAttr drvAttr =
  srcOrMain
    (\attrPath -> nixEvalET (EvalOptions Raw (Env [])) ("pkgs." <> attrPath <> ".drvAttrs." <> drvAttr))

-- Get an attribute that can be evaluated off a derivation, as in:
-- getAttr "cargoSha256" "ripgrep" -> 0lwz661rbm7kwkd6mallxym1pz8ynda5f03ynjfd16vrazy2dj21
getAttr :: MonadIO m => Raw -> Text -> Text -> ExceptT Text m Text
getAttr raw attr =
  srcOrMain
    (\attrPath -> nixEvalET (EvalOptions raw (Env [])) (attrPath <> "." <> attr))

getHash :: MonadIO m => Text -> ExceptT Text m Text
getHash =
  srcOrMain
    (\attrPath -> nixEvalET (EvalOptions Raw (Env [])) ("pkgs." <> attrPath <> ".drvAttrs.outputHash"))

getOldHash :: MonadIO m => Text -> ExceptT Text m Text
getOldHash attrPath =
  getHash attrPath

getMaintainers :: MonadIO m => Text -> ExceptT Text m Text
getMaintainers attrPath =
  nixEvalET
    (EvalOptions Raw (Env []))
    ( "(let pkgs = import ./. {}; gh = m : m.github or \"\"; nonempty = s: s != \"\"; addAt = s: \"@\"+s; in builtins.concatStringsSep \" \" (map addAt (builtins.filter nonempty (map gh pkgs."
        <> attrPath
        <> ".meta.maintainers or []))))"
    )

parseStringList :: MonadIO m => Text -> ExceptT Text m (Vector Text)
parseStringList list =
  parse nixStringList ("nix list " ++ T.unpack list) list & fmapL tshow
    & hoistEither

nixStringList :: TokenParsing m => m (Vector Text)
nixStringList = V.fromList <$> brackets (many stringLiteral)

getOutpaths :: MonadIO m => Text -> ExceptT Text m (Vector Text)
getOutpaths attrPath = do
  list <- nixEvalET (EvalOptions NoRaw (Env [("GC_INITIAL_HEAP_SIZE", "10g")])) (attrPath <> ".outputs")
  outputs <- parseStringList list
  V.sequence $ fmap (\o -> nixEvalET (EvalOptions Raw (Env [])) (attrPath <> "." <> o)) outputs

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
    (EvalOptions NoRaw (Env []))
    ( "(let pkgs = import ./. {}; in pkgs."
        <> attrPath
        <> ".meta.broken or false)"
    )
    & readNixBool

getChangelog :: MonadIO m => Text -> ExceptT Text m Text
getChangelog attrPath =
  nixEvalET
    (EvalOptions NoRaw (Env []))
    ( "(let pkgs = import ./. {}; in pkgs."
        <> attrPath
        <> ".meta.changelog or \"\")"
    )

getDescription :: MonadIO m => Text -> ExceptT Text m Text
getDescription attrPath =
  nixEvalET
    (EvalOptions NoRaw (Env []))
    ( "(let pkgs = import ./. {}; in pkgs."
        <> attrPath
        <> ".meta.description or \"\")"
    )

getHomepage ::
  Members '[P.Process, Error Text] r =>
  Text ->
  Sem r Text
getHomepage attrPath =
  fst <$> nixEvalSem
    (EvalOptions NoRaw (Env []))
    ( "(let pkgs = import ./. {}; in pkgs."
        <> attrPath
        <> ".meta.homepage or \"\")"
    )

getHomepageET :: MonadIO m => Text -> ExceptT Text m Text
getHomepageET attrPath =
  ExceptT
    . liftIO
    . runFinal
    . embedToFinal
    . Error.runError
    . Process.runIO
    $ getHomepage attrPath

getSrcUrl :: MonadIO m => Text -> ExceptT Text m Text
getSrcUrl =
  srcOrMain
    ( \attrPath ->
        nixEvalET
          (EvalOptions Raw (Env []))
          ( "(let pkgs = import ./. {}; in builtins.elemAt pkgs."
              <> attrPath
              <> ".drvAttrs.urls 0)"
          )
    )

getSrcAttr :: MonadIO m => Text -> Text -> ExceptT Text m Text
getSrcAttr attr =
  srcOrMain (\attrPath -> nixEvalET (EvalOptions NoRaw (Env [])) ("pkgs." <> attrPath <> "." <> attr))

getSrcUrls :: MonadIO m => Text -> ExceptT Text m Text
getSrcUrls = getSrcAttr "urls"

buildCmd :: Text -> ProcessConfig () () ()
buildCmd attrPath =
  silently $ proc (binPath <> "/nix-build") (nixBuildOptions ++ ["-A", attrPath & T.unpack])

log :: Text -> ProcessConfig () () ()
log attrPath = proc (binPath <> "/nix") ["log", "-f", ".", attrPath & T.unpack]

build :: MonadIO m => Text -> ExceptT Text m ()
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

cachix :: MonadIO m => Text -> ExceptT Text m ()
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
  sum $ map countUp ["sha256 =", "sha256=", "cargoSha256 =", "vendorSha256 ="]
  where
    countUp x = T.count x derivationContents

assertOldVersionOn ::
  MonadIO m => UpdateEnv -> Text -> Text -> ExceptT Text m ()
assertOldVersionOn updateEnv branchName contents =
  tryAssert
    ("Old version " <> oldVersionPattern <> " not present in " <> branchName <> " derivation file with contents: " <> contents)
    (oldVersionPattern `T.isInfixOf` contents)
  where
    oldVersionPattern = oldVersion updateEnv <> "\""

resultLink :: MonadIO m => ExceptT Text m Text
resultLink =
  T.strip
    <$> ( ourReadProcessInterleaved_ "readlink ./result"
            <|> ourReadProcessInterleaved_ "readlink ./result-bin"
        )
    <|> throwE "Could not find result link. "

sha256Zero :: Text
sha256Zero = "0000000000000000000000000000000000000000000000000000"

-- fixed-output derivation produced path '/nix/store/fg2hz90z5bc773gpsx4gfxn3l6fl66nw-source' with sha256 hash '0q1lsgc1621czrg49nmabq6am9sgxa9syxrwzlksqqr4dyzw4nmf' instead of the expected hash '0bp22mzkjy48gncj5vm9b7whzrggcbs5pd4cnb6k8jpl9j02dhdv'
getHashFromBuild :: MonadIO m => Text -> ExceptT Text m Text
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

version :: MonadIO m => ExceptT Text m Text
version = ourReadProcessInterleaved_ (proc (binPath <> "/nix") ["--version"])

getPatches :: MonadIO m => Text -> ExceptT Text m Text
getPatches attrPath =
  nixEvalET
    (EvalOptions NoRaw (Env []))
    ( "(let pkgs = import ./. {}; in (map (p: p.name) pkgs."
        <> attrPath
        <> ".patches))"
    )

hasPatchNamed :: MonadIO m => Text -> Text -> ExceptT Text m Bool
hasPatchNamed attrPath name = do
  ps <- getPatches attrPath
  return $ name `T.isInfixOf` ps

hasUpdateScript :: MonadIO m => Text -> ExceptT Text m Bool
hasUpdateScript attrPath = do
  result <-
    nixEvalET
      (EvalOptions NoRaw (Env []))
      ( "(let pkgs = import ./. {}; in builtins.hasAttr \"updateScript\" pkgs."
          <> attrPath
          <> ")"
      )
  case result of
    "true" -> return True
    _ -> return False

runUpdateScript :: MonadIO m => Text -> ExceptT Text m (ExitCode, Text)
runUpdateScript attrPath = do
  let timeout = "10m" :: Text
  (exitCode, output) <- ourReadProcessInterleaved $
    TP.setStdin (TP.byteStringInput "\n") $
    proc "timeout" [T.unpack timeout, "nix-shell", "maintainers/scripts/update.nix", "--argstr", "package", T.unpack attrPath ]
  case exitCode of
    ExitFailure 124 -> do
      return (exitCode, "updateScript for " <> attrPath <> " took longer than " <> timeout <> " and timed out. Other output: " <> output)
    _ -> do
      return (exitCode, output)
