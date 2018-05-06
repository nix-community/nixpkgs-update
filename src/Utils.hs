{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Utils
  ( Options(..)
  , Version
  , UpdateEnv(..)
  , canFail
  , checkAttrPathVersion
  , orElse
  , setupNixpkgs
  , tRead
  , parseUpdates
  , succeded
  , ExitCode(..)
  , shE
  , rewriteError
  , eitherToError
  , branchName
  ) where

import Control.Exception (Exception)
import Data.Bifunctor (first)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (FilePath)
import Shelly

default (T.Text)

type Version = Text

data Options = Options
  { dryRun :: Bool
  , workingDir :: FilePath
  , githubToken :: Text
  }

data UpdateEnv = UpdateEnv
  { packageName :: Text
  , oldVersion :: Version
  , newVersion :: Version
  , options :: Options
  }

setupNixpkgs :: Sh ()
setupNixpkgs = do
  home <- get_env_text "HOME"
  let nixpkgsPath = home </> ".cache" </> "nixpkgs"
  unlessM (test_e nixpkgsPath) $ do
    cmd "hub" "clone" "nixpkgs" nixpkgsPath -- requires that user has forked nixpkgs
    cd nixpkgsPath
    cmd "git" "remote" "add" "upstream" "https://github.com/NixOS/nixpkgs"
    cmd "git" "fetch" "upstream"
  cd nixpkgsPath
  setenv "NIX_PATH" ("nixpkgs=" <> toTextIgnore nixpkgsPath)

shE :: Sh a -> Sh (Either Text a)
shE s = do
  r <- canFail s
  status <- lastExitCode
  case status of
    0 -> return $ Right r
    c -> return $ Left ("Exit code: " <> T.pack (show c))

rewriteError :: Text -> Sh (Either Text a) -> Sh (Either Text a)
rewriteError t = fmap (first (const t))

eitherToError :: (Text -> Sh a) -> Sh (Either Text a) -> Sh a
eitherToError errorExit s = do
  e <- s
  either errorExit return e

canFail :: Sh a -> Sh a
canFail = errExit False

succeded :: Sh a -> Sh Bool
succeded s = do
  canFail s
  status <- lastExitCode
  return (status == 0)

orElse :: Sh a -> Sh a -> Sh a
orElse a b = do
  v <- canFail a
  status <- lastExitCode
  if status == 0
    then return v
    else b

infixl 3 `orElse`

branchName :: UpdateEnv -> Text
branchName ue = "auto-update/" <> packageName ue

parseUpdates :: Text -> [Either Text (Text, Version, Version)]
parseUpdates = map (toTriple . T.words) . T.lines
  where
    toTriple :: [Text] -> Either Text (Text, Version, Version)
    toTriple [package, oldVersion, newVersion] =
      Right (package, oldVersion, newVersion)
    toTriple line = Left $ "Unable to parse update: " <> T.unwords line

tRead :: Read a => Text -> a
tRead = read . T.unpack

notElemOf :: (Eq a, Foldable t) => t a -> a -> Bool
notElemOf options = not . flip elem options

-- | Similar to @breakOn@, but will not keep the pattern at the beginning of the suffix.
--
-- Examples:
--
-- > breakOn "::" "a::b::c"
-- ("a","b::c")
clearBreakOn :: Text -> Text -> (Text, Text)
clearBreakOn boundary string =
  let (prefix, suffix) = T.breakOn boundary string
   in if T.null suffix
        then (prefix, suffix)
        else (prefix, T.drop (T.length boundary) suffix)

-- | Check if attribute path is not pinned to a certain version.
-- If a derivation is expected to stay at certain version branch,
-- it will usually have the branch as a part of the attribute path.
--
-- Examples:
--
-- >>> checkAttrPathVersion "libgit2_0_25" "0.25.3"
-- True
--
-- >>> checkAttrPathVersion "owncloud90" "9.0.3"
-- True
--
-- >>> checkAttrPathVersion "owncloud-client" "2.4.1"
-- True
--
-- >>> checkAttrPathVersion "owncloud90" "9.1.3"
-- False
checkAttrPathVersion :: Text -> Version -> Bool
checkAttrPathVersion attrPath newVersion =
  if "_" `T.isInfixOf` attrPath
    then let attrVersionPart =
               let (name, version) = clearBreakOn "_" attrPath
                in if T.any (notElemOf ('_' : ['0' .. '9'])) version
                     then Nothing
                     else Just version
        -- Check assuming version part has underscore separators
             attrVersionPeriods = T.replace "_" "." <$> attrVersionPart
        -- If we don't find version numbers in the attr path, exit success.
          in maybe True (`T.isPrefixOf` newVersion) attrVersionPeriods
         -- other path
    else let attrVersionPart =
               let version = T.dropWhile (notElemOf ['0' .. '9']) attrPath
                in if T.any (notElemOf ['0' .. '9']) version
                     then Nothing
                     else Just version
        -- Check assuming version part is the prefix of the version with dots
        -- removed. For example, 91 => "9.1"
             noPeriodNewVersion = T.replace "." "" newVersion
            -- If we don't find version numbers in the attr path, exit success.
          in maybe True (`T.isPrefixOf` noPeriodNewVersion) attrVersionPart

newtype ExitCode =
  ExitCode Int
  deriving (Show)

instance Exception ExitCode
