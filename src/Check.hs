{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Check
  ( result,
  )
where

import Control.Applicative (many)
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Language.Haskell.TH.Env (envQ)
import OurPrelude
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit
import System.IO.Temp (withSystemTempDirectory)
import Text.Regex.Applicative.Text (RE', (=~))
import qualified Text.Regex.Applicative.Text as RE
import Utils (UpdateEnv (..), Version, nixBuildOptions)

default (T.Text)

treeBin :: String
treeBin = fromJust ($$(envQ "TREE") :: Maybe String) <> "/bin/tree"

procTree :: [String] -> ProcessConfig () () ()
procTree = proc treeBin

gistBin :: String
gistBin = fromJust ($$(envQ "GIST") :: Maybe String) <> "/bin/gist"

procGist :: [String] -> ProcessConfig () () ()
procGist = proc gistBin

data BinaryCheck = BinaryCheck
  { filePath :: FilePath,
    zeroExitCode :: Bool,
    versionPresent :: Bool
  }

-- | Construct regex: [^\.]*${version}\.*\s*
versionRegex :: Text -> RE' ()
versionRegex version =
  (\_ _ _ _ -> ()) <$> many (RE.psym (/= '.')) <*> RE.string version
    <*> many (RE.sym '.')
    <*> many (RE.psym isSpace)

checkTestsBuild :: Text -> IO Bool
checkTestsBuild attrPath =
  let args =
        nixBuildOptions
          ++ [ "-E",
               "{ config }: (import ./. { inherit config; })."
                 ++ (T.unpack attrPath)
                 ++ ".tests or {}"
             ]
   in do
        r <- runExceptT $ ourReadProcessInterleaved $ proc "nix-build" args
        case r of
          Right (ExitSuccess, _) -> return True
          _ -> return False

-- | Run a program with provided argument and report whether the output
-- mentions the expected version
checkBinary :: Text -> Version -> FilePath -> IO BinaryCheck
checkBinary argument expectedVersion program = do
  eResult <-
    runExceptT $
      withSystemTempDirectory
        ("nixpkgs-update-" <> program)
        ( ourLockedDownReadProcessInterleaved $
            shell ("timeout -k 2 1 " <> program <> " " <> T.unpack argument)
        )
  case eResult of
    Left (_ :: Text) -> return $ BinaryCheck program False False
    Right (exitCode, contents) -> do
      let hasVersion = isJust $ contents =~ versionRegex expectedVersion
      return $ BinaryCheck program (exitCode == ExitSuccess) hasVersion

checks :: [Version -> FilePath -> IO BinaryCheck]
checks =
  [ checkBinary "",
    checkBinary "-V",
    checkBinary "-v",
    checkBinary "--version",
    checkBinary "version",
    checkBinary "-h",
    checkBinary "--help",
    checkBinary "help"
  ]

someChecks :: BinaryCheck -> [IO BinaryCheck] -> IO BinaryCheck
someChecks best [] = return best
someChecks best (c : rest) = do
  current <- c
  let nb = newBest current
  case nb of
    BinaryCheck _ True True -> return nb
    _ -> someChecks nb rest
  where
    newBest :: BinaryCheck -> BinaryCheck
    newBest (BinaryCheck _ currentExit currentVersionPresent) =
      BinaryCheck
        (filePath best)
        (zeroExitCode best || currentExit)
        (versionPresent best || currentVersionPresent)

-- | Run a program with various version or help flags and report
-- when they succeded
runChecks :: Version -> FilePath -> IO BinaryCheck
runChecks expectedVersion program =
  someChecks (BinaryCheck program False False) checks'
  where
    checks' = map (\c -> c expectedVersion program) checks

checkTestsBuildReport :: Bool -> Text
checkTestsBuildReport False =
  "- Warning: a test defined in `passthru.tests` did not pass"
checkTestsBuildReport True =
  "- The tests defined in `passthru.tests`, if any, passed"

checkReport :: BinaryCheck -> Text
checkReport (BinaryCheck p False False) =
  "- Warning: no invocation of "
    <> T.pack p
    <> " had a zero exit code or showed the expected version"
checkReport (BinaryCheck p _ _) =
  "- " <> T.pack p <> " passed the binary check."

ourLockedDownReadProcessInterleaved ::
  MonadIO m =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  FilePath ->
  ExceptT Text m (ExitCode, Text)
ourLockedDownReadProcessInterleaved processConfig tempDir =
  processConfig & setWorkingDir tempDir
    & setEnv [("EDITOR", "echo"), ("HOME", "/we-dont-write-to-home")]
    & ourReadProcessInterleaved

foundVersionInOutputs :: Text -> String -> IO (Maybe Text)
foundVersionInOutputs expectedVersion resultPath =
  hush
    <$> runExceptT
      ( do
          (exitCode, _) <-
            proc "grep" ["-r", T.unpack expectedVersion, resultPath]
              & ourReadProcessInterleaved
          case exitCode of
            ExitSuccess ->
              return $
                "- found "
                  <> expectedVersion
                  <> " with grep in "
                  <> T.pack resultPath
                  <> "\n"
            _ -> throwE "grep did not find version in file names"
      )

foundVersionInFileNames :: Text -> String -> IO (Maybe Text)
foundVersionInFileNames expectedVersion resultPath =
  hush
    <$> runExceptT
      ( do
          (_, contents) <-
            shell ("find " <> resultPath) & ourReadProcessInterleaved
          (contents =~ versionRegex expectedVersion) & hoistMaybe
            & noteT (T.pack "Expected version not found")
          return $
            "- found "
              <> expectedVersion
              <> " in filename of file in "
              <> T.pack resultPath
              <> "\n"
      )

treeGist :: String -> IO (Maybe Text)
treeGist resultPath =
  hush
    <$> runExceptT
      ( do
          contents <- procTree [resultPath] & ourReadProcessInterleavedBS_
          g <-
            shell gistBin & setStdin (byteStringInput contents)
              & ourReadProcessInterleaved_
          return $ "- directory tree listing: " <> g <> "\n"
      )

duGist :: String -> IO (Maybe Text)
duGist resultPath =
  hush
    <$> runExceptT
      ( do
          contents <- proc "du" [resultPath] & ourReadProcessInterleavedBS_
          g <-
            shell gistBin & setStdin (byteStringInput contents)
              & ourReadProcessInterleaved_
          return $ "- du listing: " <> g <> "\n"
      )

result :: MonadIO m => UpdateEnv -> String -> m Text
result updateEnv resultPath =
  liftIO $ do
    let expectedVersion = newVersion updateEnv
        binaryDir = resultPath <> "/bin"
    testsBuild <- checkTestsBuild (packageName updateEnv)
    binExists <- doesDirectoryExist binaryDir
    binaries <-
      if binExists
        then
          ( do
              fs <- listDirectory binaryDir
              filterM doesFileExist fs
          )
        else return []
    checks' <- forM binaries $ \binary -> runChecks expectedVersion binary
    let passedZeroExitCode =
          (T.pack . show)
            ( foldl
                ( \acc c ->
                    if zeroExitCode c
                      then acc + 1
                      else acc
                )
                0
                checks' ::
                Int
            )
        passedVersionPresent =
          (T.pack . show)
            ( foldl
                ( \acc c ->
                    if versionPresent c
                      then acc + 1
                      else acc
                )
                0
                checks' ::
                Int
            )
        numBinaries = (T.pack . show) (length binaries)
    someReports <-
      fromMaybe ""
        <$> foundVersionInOutputs expectedVersion resultPath
        <> foundVersionInFileNames expectedVersion resultPath
        <> treeGist resultPath
        <> duGist resultPath
    return $
      let testsBuildSummary = checkTestsBuildReport testsBuild
          c = T.intercalate "\n" (map checkReport checks')
          binaryCheckSummary =
            "- "
              <> passedZeroExitCode
              <> " of "
              <> numBinaries
              <> " passed binary check by having a zero exit code."
          versionPresentSummary =
            "- "
              <> passedVersionPresent
              <> " of "
              <> numBinaries
              <> " passed binary check by having the new version present in output."
       in [interpolate|
              $testsBuildSummary
              $c
              $binaryCheckSummary
              $versionPresentSummary
              $someReports
            |]
