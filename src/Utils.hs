{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Utils
  ( Boundary (..),
    Options (..),
    ProductID,
    URL,
    UpdateEnv (..),
    packageNameL,
    oldVersionL,
    newVersionL,
    sourceURLL,
    optionsL,
    Version,
    VersionMatcher (..),
    branchName,
    branchPrefix,
    getGithubToken,
    getGithubUser,
    logDir,
    nixBuildOptions,
    nixCommonOptions,
    overwriteErrorT,
    parseUpdates,
    prTitle,
    runLog,
    srcOrMain,
    stripQuotes,
    whenBatch,
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
import qualified GitHub as GH
import OurPrelude
import Polysemy.Output
import RIO
import qualified RIO.List as L
import System.Directory (doesDirectoryExist)
import System.Posix.Directory (createDirectory)
import System.Posix.Env (getEnv)
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
import Text.Read (readEither)
import qualified Prelude

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

data Options = Options
  { doPR :: Bool,
    batchUpdate :: Bool,
    githubUser :: GH.Name GH.Owner,
    githubToken :: Text,
    makeCVEReport :: Bool,
    runNixpkgsReview :: Bool,
    calculateOutpaths :: Bool
  }
  deriving (Show)

data UpdateEnv = UpdateEnv
  { packageName :: !Text,
    oldVersion :: !Version,
    newVersion :: !Version,
    sourceURL :: !(Maybe URL),
    options :: !Options,
    updateEnvLogFunc :: !LogFunc
  }

class HasPackageName env where
  packageNameL :: Lens' env Text

instance HasPackageName UpdateEnv where
  packageNameL = lens packageName (\x y -> x {packageName = y})

class HasOldVersion env where
  oldVersionL :: Lens' env Version

instance HasOldVersion UpdateEnv where
  oldVersionL = lens oldVersion (\x y -> x {oldVersion = y})

class HasNewVersion env where
  newVersionL :: Lens' env Version

instance HasNewVersion UpdateEnv where
  newVersionL = lens newVersion (\x y -> x {newVersion = y})

class HasSourceURL env where
  sourceURLL :: Lens' env (Maybe URL)

instance HasSourceURL UpdateEnv where
  sourceURLL = lens sourceURL (\x y -> x {sourceURL = y})

class HasOptions env where
  optionsL :: Lens' env Options

instance HasOptions UpdateEnv where
  optionsL = lens options (\x y -> x {options = y})

instance HasLogFunc UpdateEnv where
  logFuncL = lens updateEnvLogFunc (\x y -> x {updateEnvLogFunc = y})

whenBatch :: Applicative f => UpdateEnv -> f () -> f ()
whenBatch updateEnv = when (batchUpdate . options $ updateEnv)

prTitle :: UpdateEnv -> Text -> Text
prTitle updateEnv attrPath =
  let oV = oldVersion updateEnv
      nV = newVersion updateEnv
   in T.strip [interpolate| $attrPath: $oV -> $nV |]

regDirMode :: FileMode
regDirMode =
  directoryMode .|. ownerModes .|. groupModes .|. otherReadMode
    .|. otherExecuteMode

logsDirectory :: MonadIO m => m FilePath
logsDirectory = do
  dir <- getEnv "LOGS_DIRECTORY"
    noteT "Could not get environment variable LOGS_DIRECTORY" $
      MaybeT $
        liftIO $
        
  dirExists <- liftIO $ doesDirectoryExist dir
  tryAssert ("LOGS_DIRECTORY " <> T.pack dir <> " does not exist.") dirExists
  unless
    dirExists
    ( liftIO $
        Prelude.putStrLn "creating xdgRuntimeDir" >> createDirectory dir regDirMode
    )
  return dir

xdgRuntimeDir :: MonadIO m => ExceptT Text m FilePath
xdgRuntimeDir = do
  xDir <-
    noteT "Could not get environment variable XDG_RUNTIME_DIR" $
      MaybeT $
        liftIO $
          getEnv "XDG_RUNTIME_DIR"
  xDirExists <- liftIO $ doesDirectoryExist xDir
  tryAssert ("XDG_RUNTIME_DIR " <> T.pack xDir <> " does not exist.") xDirExists
  let dir = xDir <> "/nixpkgs-update"
  dirExists <- liftIO $ fileExist dir
  unless
    dirExists
    ( liftIO $
        Prelude.putStrLn "creating xdgRuntimeDir" >> createDirectory dir regDirMode
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

hubConfigField :: Text -> IO (Maybe Text)
hubConfigField field = do
  hubFile <- hubFileLocation
  case hubFile of
    Nothing -> return Nothing
    Just file -> do
      exists <- fileExist file
      if not exists
        then return Nothing
        else do
          contents <- T.readFile file
          let splits = T.splitOn field contents
              token = fmap (T.takeWhile (/= '\n')) $ L.headMaybe (drop 1 splits)
          return $ token

getGithubToken :: IO Text
getGithubToken = do
  et <- envToken
  lt <- localToken
  ht <- hubConfigField "oauth_token: "
  return $ fromJust (et <|> lt <|> ht)

getGithubUser :: IO (GH.Name GH.Owner)
getGithubUser = do
  hubUser <- hubConfigField "user: "
  case hubUser of
    Just usr -> return $ GH.mkOwnerName usr
    Nothing -> return $ GH.mkOwnerName "r-ryantm"

stripQuotes :: Text -> Maybe Text
stripQuotes = T.stripPrefix "\"" >=> T.stripSuffix "\""
