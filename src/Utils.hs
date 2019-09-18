{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Utils
  ( Options(..)
  , ProductID
  , Version
  , Boundary(..)
  , VersionMatcher(..)
  , UpdateEnv(..)
  , setupNixpkgs
  , tRead
  , parseUpdates
  , overwriteErrorT
  , branchName
  , branchPrefix
  , runtimeDir
  , srcOrMain
  , prTitle
  , nixBuildOptions
  ) where

import OurPrelude

import Data.Bits ((.|.))
import qualified Data.Text as T
import Database.SQLite.Simple (ResultError(..), SQLData(..))
import Database.SQLite.Simple.FromField
  ( FieldParser
  , FromField
  , fromField
  , returnError
  )
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.Ok (Ok(..))
import Database.SQLite.Simple.ToField (ToField, toField)
import System.Directory (doesDirectoryExist, setCurrentDirectory)
import System.Environment.XDG.BaseDir
import System.Posix.Directory (createDirectory)
import System.Posix.Env (getEnv, setEnv)
import System.Posix.Files
  ( directoryMode
  , fileExist
  , groupModes
  , otherExecuteMode
  , otherReadMode
  , ownerModes
  )
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (FileMode)
import qualified System.Process.Typed
import Text.Read (readEither)
import Type.Reflection (Typeable)

default (T.Text)

type ProductID = Text

type Version = Text

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
  = ExactMatcher Version
  | FuzzyMatcher Version
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

data Options =
  Options
    { dryRun :: Bool
    , githubToken :: Text
    }
  deriving (Show)

data UpdateEnv =
  UpdateEnv
    { packageName :: Text
    , oldVersion :: Version
    , newVersion :: Version
    , options :: Options
    }

prTitle :: UpdateEnv -> Text -> Text
prTitle updateEnv attrPath =
  let oV = oldVersion updateEnv
      nV = newVersion updateEnv
   in [interpolate| $attrPath: $oV -> $nV |]

regDirMode :: FileMode
regDirMode =
  directoryMode .|. ownerModes .|. groupModes .|. otherReadMode .|.
  otherExecuteMode

xdgRuntimeDir :: MonadIO m => ExceptT Text m FilePath
xdgRuntimeDir = do
  xDir <-
    noteT "Could not get environment variable XDG_RUNTIME_DIR" $
    MaybeT $ liftIO $ getEnv "XDG_RUNTIME_DIR"
  xDirExists <- liftIO $ doesDirectoryExist xDir
  tryAssert ("XDG_RUNTIME_DIR " <> T.pack xDir <> " does not exist.") xDirExists
  let dir = xDir <> "/nixpkgs-update"
  dirExists <- liftIO $ fileExist dir
  unless
    dirExists
    (liftIO $
     putStrLn "creating xdgRuntimeDir" >> createDirectory dir regDirMode)
  return dir

tmpRuntimeDir :: MonadIO m => ExceptT Text m FilePath
tmpRuntimeDir = do
  dir <- liftIO $ mkdtemp "nixpkgs-update"
  dirExists <- liftIO $ doesDirectoryExist dir
  tryAssert
    ("Temporary directory " <> T.pack dir <> " does not exist.")
    dirExists
  return dir

runtimeDir :: IO FilePath
runtimeDir = do
  r <-
    runExceptT
      (xdgRuntimeDir <|> tmpRuntimeDir <|>
       throwE
         "Failed to create runtimeDir from XDG_RUNTIME_DIR or tmp directory")
  case r of
    Right dir -> return dir
    Left e -> error $ T.unpack e

setupNixpkgs :: Text -> IO ()
setupNixpkgs githubt = do
  fp <- getUserCacheDir "nixpkgs"
  exists <- doesDirectoryExist fp
  unless exists $ do
    proc "hub" ["clone", "nixpkgs", fp] & -- requires that user has forked nixpkgs
      System.Process.Typed.setEnv
        [("GITHUB_TOKEN" :: String, githubt & T.unpack)] &
      runProcess_
    setCurrentDirectory fp
    shell "git remote add upstream https://github.com/NixOS/nixpkgs" &
      runProcess_
    shell "git fetch upstream" & runProcess_
  setCurrentDirectory fp
  setEnv "NIX_PATH" ("nixpkgs=" <> fp) True

overwriteErrorT :: MonadIO m => Text -> ExceptT Text m a -> ExceptT Text m a
overwriteErrorT t = fmapLT (const t)

branchPrefix :: Text
branchPrefix = "auto-update/"

branchName :: UpdateEnv -> Text
branchName ue = branchPrefix <> packageName ue

parseUpdates :: Text -> [Either Text (Text, Version, Version)]
parseUpdates = map (toTriple . T.words) . T.lines
  where
    toTriple :: [Text] -> Either Text (Text, Version, Version)
    toTriple [package, oldVer, newVer] = Right (package, oldVer, newVer)
    toTriple line = Left $ "Unable to parse update: " <> T.unwords line

tRead :: Read a => Text -> a
tRead = read . T.unpack

srcOrMain :: MonadIO m => (Text -> ExceptT Text m a) -> Text -> ExceptT Text m a
srcOrMain et attrPath = et (attrPath <> ".src") <|> et attrPath

nixBuildOptions :: [String]
nixBuildOptions =
  [ "--option"
  , "sandbox"
  , "true"
  , "--option"
  , "restrict-eval"
  , "true"
  , "--arg"
  , "config"
  , "{ allowBroken = true; allowUnfree = true; allowAliases = false; }"
  ]
