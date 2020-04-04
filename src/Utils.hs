{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Utils
  ( Options (..),
    ProductID,
    Version,
    Boundary (..),
    VersionMatcher (..),
    UpdateEnv (..),
    URL,
    setupNixpkgs,
    tRead,
    parseUpdates,
    overwriteErrorT,
    branchName,
    branchPrefix,
    logDir,
    srcOrMain,
    prTitle,
    nixBuildOptions,
    nixCommonOptions,
    runLog,
    getGithubToken,
  )
where

import Data.Bits ((.|.))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Database.SQLite.Simple (ResultError (..), SQLData (..))
import Database.SQLite.Simple.FromField
  ( FieldParser,
    FromField,
    fromField,
    returnError,
  )
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField, toField)
import OurPrelude
import Polysemy.Output
import System.Directory (doesDirectoryExist, setCurrentDirectory)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Posix.Directory (createDirectory)
import System.Posix.Env (getEnv, setEnv)
import System.Posix.Files
  ( directoryMode,
    fileExist,
    groupModes,
    otherExecuteMode,
    otherReadMode,
    ownerModes,
  )
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (FileMode)
import qualified System.Process.Typed
import Text.Read (readEither)
import Type.Reflection (Typeable)

default (T.Text)

type ProductID = Text

type Version = Text

type URL = Text

-- | The Ord instance is used to sort lists of matchers in order to compare them
-- as a set, it is not useful for comparing bounds since the ordering of bounds
-- depends on whether it is a start or end bound.
data Boundary a
  = Unbounded
  | Including a
  | Excluding a
  deriving (Eq, Ord, Show, Read)

-- | The Ord instance is used to sort lists of matchers in order to compare them
-- as a set, it is not useful for comparing versions.
data VersionMatcher
  = SingleMatcher Version
  | RangeMatcher (Boundary Version) (Boundary Version)
  deriving (Eq, Ord, Show, Read)

readField :: (Read a, Typeable a) => FieldParser a
readField f@(Field (SQLText t) _) =
  case readEither (T.unpack t) of
    Right x -> Ok x
    Left e -> returnError ConversionFailed f $ "read error: " <> e
readField f = returnError ConversionFailed f "expecting SQLText column type"

showField :: Show a => a -> SQLData
showField = toField . show

instance FromField VersionMatcher where
  fromField = readField

instance ToField VersionMatcher where
  toField = showField

data Options
  = Options
      { dryRun :: Bool,
        githubToken :: Text,
        pushToCachix :: Bool,
        calculateOutpaths :: Bool
      }
  deriving (Show)

data UpdateEnv
  = UpdateEnv
      { packageName :: Text,
        oldVersion :: Version,
        newVersion :: Version,
        sourceURL :: Maybe URL,
        options :: Options
      }

prTitle :: UpdateEnv -> Text -> Text
prTitle updateEnv attrPath =
  let oV = oldVersion updateEnv
      nV = newVersion updateEnv
   in [interpolate| $attrPath: $oV -> $nV |]

regDirMode :: FileMode
regDirMode =
  directoryMode .|. ownerModes .|. groupModes .|. otherReadMode
    .|. otherExecuteMode

logsDirectory :: MonadIO m => ExceptT Text m FilePath
logsDirectory = do
  dir <-
    noteT "Could not get environment variable LOGS_DIRECTORY"
      $ MaybeT
      $ liftIO
      $ getEnv "LOGS_DIRECTORY"
  dirExists <- liftIO $ doesDirectoryExist dir
  tryAssert ("LOGS_DIRECTORY " <> T.pack dir <> " does not exist.") dirExists
  unless
    dirExists
    ( liftIO $
        putStrLn "creating xdgRuntimeDir" >> createDirectory dir regDirMode
    )
  return dir

xdgRuntimeDir :: MonadIO m => ExceptT Text m FilePath
xdgRuntimeDir = do
  xDir <-
    noteT "Could not get environment variable XDG_RUNTIME_DIR"
      $ MaybeT
      $ liftIO
      $ getEnv "XDG_RUNTIME_DIR"
  xDirExists <- liftIO $ doesDirectoryExist xDir
  tryAssert ("XDG_RUNTIME_DIR " <> T.pack xDir <> " does not exist.") xDirExists
  let dir = xDir <> "/nixpkgs-update"
  dirExists <- liftIO $ fileExist dir
  unless
    dirExists
    ( liftIO $
        putStrLn "creating xdgRuntimeDir" >> createDirectory dir regDirMode
    )
  return dir

tmpRuntimeDir :: MonadIO m => ExceptT Text m FilePath
tmpRuntimeDir = do
  dir <- liftIO $ mkdtemp "nixpkgs-update"
  dirExists <- liftIO $ doesDirectoryExist dir
  tryAssert
    ("Temporary directory " <> T.pack dir <> " does not exist.")
    dirExists
  return dir

logDir :: IO FilePath
logDir = do
  r <-
    runExceptT
      ( logsDirectory <|> xdgRuntimeDir <|> tmpRuntimeDir
          <|> throwE
            "Failed to create log directory."
      )
  case r of
    Right dir -> return dir
    Left e -> error $ T.unpack e

setupNixpkgs :: Text -> IO ()
setupNixpkgs githubt = do
  fp <- getUserCacheDir "nixpkgs"
  exists <- doesDirectoryExist fp
  unless exists $ do
    proc "hub" ["clone", "nixpkgs", fp]
      & System.Process.Typed.setEnv -- requires that user has forked nixpkgs
        [("GITHUB_TOKEN" :: String, githubt & T.unpack)]
      & runProcess_
    setCurrentDirectory fp
    shell "git remote add upstream https://github.com/NixOS/nixpkgs"
      & runProcess_
    shell "git fetch upstream" & runProcess_
  setCurrentDirectory fp
  System.Posix.Env.setEnv "NIX_PATH" ("nixpkgs=" <> fp) True

overwriteErrorT :: MonadIO m => Text -> ExceptT Text m a -> ExceptT Text m a
overwriteErrorT t = fmapLT (const t)

branchPrefix :: Text
branchPrefix = "auto-update/"

branchName :: UpdateEnv -> Text
branchName ue = branchPrefix <> packageName ue

parseUpdates :: Text -> [Either Text (Text, Version, Version, Maybe URL)]
parseUpdates = map (toTriple . T.words) . T.lines
  where
    toTriple :: [Text] -> Either Text (Text, Version, Version, Maybe URL)
    toTriple [package, oldVer, newVer] = Right (package, oldVer, newVer, Nothing)
    toTriple [package, oldVer, newVer, url] = Right (package, oldVer, newVer, Just url)
    toTriple line = Left $ "Unable to parse update: " <> T.unwords line

tRead :: Read a => Text -> a
tRead = read . T.unpack

srcOrMain :: MonadIO m => (Text -> ExceptT Text m a) -> Text -> ExceptT Text m a
srcOrMain et attrPath = et (attrPath <> ".src") <|> et attrPath

nixCommonOptions :: [String]
nixCommonOptions =
  [ "--arg",
    "config",
    "{ allowBroken = true; allowUnfree = true; allowAliases = false; }",
    "--arg",
    "overlays",
    "[ ]"
  ]

nixBuildOptions :: [String]
nixBuildOptions =
  [ "--option",
    "sandbox",
    "true",
    "--option",
    "restrict-eval",
    "true"
  ]
    <> nixCommonOptions

runLog ::
  Member (Embed IO) r =>
  (Text -> IO ()) ->
  Sem ((Output Text) ': r) a ->
  Sem r a
runLog logger =
  interpret \case
    Output o -> embed $ logger o

envToken :: IO (Maybe Text)
envToken = fmap tshow <$> getEnv "GITHUB_TOKEN"

localToken :: IO (Maybe Text)
localToken = do
  exists <- fileExist "github_token.txt"
  if exists
    then (Just . T.strip <$> T.readFile "github_token.txt")
    else (return Nothing)

hubFileLocation :: IO (Maybe FilePath)
hubFileLocation = do
  xloc <- fmap (<> "/hub") <$> getEnv "XDG_CONFIG_HOME"
  hloc <- fmap (<> "/.config/hub") <$> getEnv "HOME"
  return (xloc <|> hloc)

hubConfigToken :: IO (Maybe Text)
hubConfigToken = do
  hubFile <- hubFileLocation
  case hubFile of
    Nothing -> return Nothing
    Just file -> do
      exists <- fileExist file
      if not exists
        then return Nothing
        else do
          contents <- T.readFile file
          let splits = T.splitOn "oauth_token: " contents
              token = T.takeWhile (/= '\n') $ head (drop 1 splits)
          return $ Just token

getGithubToken :: IO Text
getGithubToken = do
  et <- envToken
  lt <- localToken
  ht <- hubConfigToken
  return $ fromJust (et <|> lt <|> ht)
