{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Check
  ( result,
    -- exposed for testing:
    hasVersion
  )
where

import Control.Applicative (many)
import Data.Char (isDigit, isLetter)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.TH.Env (envQ)
import OurPrelude
import System.Exit
import Text.Regex.Applicative.Text (RE', (=~))
import qualified Text.Regex.Applicative.Text as RE
import Utils (UpdateEnv (..), nixBuildOptions)

default (T.Text)

treeBin :: String
treeBin = fromJust ($$(envQ "TREE") :: Maybe String) <> "/bin/tree"

procTree :: [String] -> ProcessConfig () () ()
procTree = proc treeBin

gistBin :: String
gistBin = fromJust ($$(envQ "GIST") :: Maybe String) <> "/bin/gist"

procGist :: [String] -> ProcessConfig () () ()
procGist = proc gistBin

timeoutBin :: String
timeoutBin = fromJust ($$(envQ "TIMEOUT") :: Maybe String) <> "/bin/timeout"

data BinaryCheck = BinaryCheck
  { filePath :: FilePath,
    zeroExitCode :: Bool,
    versionPresent :: Bool
  }

isWordCharacter :: Char -> Bool
isWordCharacter c = (isDigit c) || (isLetter c)

isNonWordCharacter :: Char -> Bool
isNonWordCharacter c = not (isWordCharacter c)

-- | Construct regex: /.*\b${version}\b.*/s
versionRegex :: Text -> RE' ()
versionRegex version =
  (\_ -> ()) <$> (
    (((many RE.anySym) <* (RE.psym isNonWordCharacter)) <|> (RE.pure ""))
    *> (RE.string version) <*
    ((RE.pure "") <|> ((RE.psym isNonWordCharacter) *> (many RE.anySym)))
  )

hasVersion :: Text -> Text -> Bool
hasVersion contents expectedVersion =
  isJust $ contents =~ versionRegex expectedVersion

checkTestsBuild :: Text -> IO Bool
checkTestsBuild attrPath = do
  let timeout = "10m"
  let
    args =
        [ T.unpack timeout, "nix-build" ] ++
        nixBuildOptions
          ++ [ "-E",
               "{ config }: (import ./. { inherit config; })."
                 ++ (T.unpack attrPath)
                 ++ ".tests or {}"
             ]
  r <- runExceptT $ ourReadProcessInterleaved $ proc "timeout" args
  case r of
    Left errorMessage -> do
      T.putStrLn $ attrPath <> ".tests process failed with output: " <> errorMessage
      return False
    Right (exitCode, output) -> do
      case exitCode of
        ExitFailure 124 -> do
          T.putStrLn $ attrPath <> ".tests took longer than " <> timeout <> " and timed out. Other output: " <> output
          return False
        ExitSuccess -> return True
        _ -> return False

checkTestsBuildReport :: Bool -> Text
checkTestsBuildReport False =
  "- Warning: a test defined in `passthru.tests` did not pass"
checkTestsBuildReport True =
  "- The tests defined in `passthru.tests`, if any, passed"

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
    testsBuild <- checkTestsBuild (packageName updateEnv)
    someReports <-
      fromMaybe ""
        <$> foundVersionInOutputs expectedVersion resultPath
        <> foundVersionInFileNames expectedVersion resultPath
        <> treeGist resultPath
        <> duGist resultPath
    return $
      let testsBuildSummary = checkTestsBuildReport testsBuild
       in [interpolate|
              $testsBuildSummary
              $someReports
            |]
