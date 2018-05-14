{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Nix
  ( nixEvalE
  , compareVersions
  , lookupAttrPath
  , getDerivationFile
  , getMaintainers
  , getOldHash
  , getSrcUrl
  , getSrcUrls
  , getIsBroken
  , nixBuild
  , getDescription
  , Raw(..)
  ) where

import Control.Category ((>>>))
import Control.Error (headMay)
import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (FilePath)
import Shelly (FilePath, Sh, cmd, fromText, run)
import Utils (UpdateEnv(..), rewriteError, shE)

data Raw
  = Raw
  | NoRaw

rawOpt :: Raw -> [Text]
rawOpt Raw = ["--raw"]
rawOpt NoRaw = []

nixEvalE :: Raw -> Text -> Sh (Either Text Text)
nixEvalE raw expr =
  run "nix" (["eval", "-f", "."] <> rawOpt raw <> [expr]) &
  (fmap T.strip >>> shE >>> rewriteError ("nix eval failed for " <> expr))

-- Error if the "new version" is actually newer according to nix
compareVersions :: UpdateEnv -> Sh (Either Text ())
compareVersions updateEnv = do
  versionComparison <-
    nixEvalE
      NoRaw
      ("(builtins.compareVersions \"" <> newVersion updateEnv <> "\" \"" <>
       oldVersion updateEnv <>
       "\")")
  return $
    case versionComparison of
      Right "1" -> Right ()
      Right a ->
        Left $
        newVersion updateEnv <> " is not newer than " <> oldVersion updateEnv <>
        " according to Nix; versionComparison: " <>
        a
      Left a -> Left a
 -- This is extremely slow but gives us the best results we know of

lookupAttrPath :: UpdateEnv -> Sh (Either Text Text)
lookupAttrPath updateEnv =
  cmd
    "nix-env"
    "-qa"
    (packageName updateEnv <> "-" <> oldVersion updateEnv)
    "-f"
    "."
    "--attr-path" &
  (fmap (head . T.words . head . T.lines) >>>
   shE >>>
   rewriteError "nix-env -q failed to find package name with old version")

getDerivationFile :: UpdateEnv -> Text -> Sh (Either Text FilePath)
getDerivationFile updateEnv attrPath =
  cmd "env" "EDITOR=echo" "nix" "edit" attrPath "-f" "." &
  (fmap T.strip >>>
   fmap fromText >>> shE >>> rewriteError "Couldn't find derivation file.")

getHash :: Text -> Sh (Either Text Text)
getHash attrPath =
  nixEvalE Raw ("pkgs." <> attrPath <> ".src.drvAttrs.outputHash")

getOldHash :: Text -> Sh (Either Text Text)
getOldHash attrPath =
  getHash attrPath &
  rewriteError
    ("Could not find old output hash at " <> attrPath <>
     ".src.drvAttrs.outputHash.")

getSrcUrl :: Text -> Sh (Either Text Text)
getSrcUrl attrPath =
  nixEvalE
    Raw
    ("(let pkgs = import ./. {}; in builtins.elemAt pkgs." <> attrPath <>
     ".src.drvAttrs.urls 0)")

getMaintainers :: Text -> Sh (Either Text Text)
getMaintainers attrPath =
  nixEvalE
    Raw
    ("(let pkgs = import ./. {}; gh = m : m.github or \"\"; nonempty = s: s != \"\"; addAt = s: \"@\"+s; in builtins.concatStringsSep \" \" (map addAt (builtins.filter nonempty (map gh pkgs." <>
     attrPath <>
     ".meta.maintainers or []))))") &
  rewriteError ("Could not fetch maintainers for" <> attrPath)

getIsBroken :: Text -> Sh (Either Text Text)
getIsBroken attrPath =
  nixEvalE
    NoRaw
    ("(let pkgs = import ./. {}; in pkgs." <> attrPath <>
     ".meta.broken or false)") &
  rewriteError ("Could not get meta.broken for attrpath " <> attrPath)

getDescription :: Text -> Sh (Either Text Text)
getDescription attrPath =
  nixEvalE
    NoRaw
    ("(let pkgs = import ./. {}; in pkgs." <> attrPath <>
     ".meta.description or '')") &
  rewriteError ("Could not get meta.description for attrpath " <> attrPath)

getSrcUrls :: Text -> Sh (Either Text Text)
getSrcUrls attrPath = nixEvalE NoRaw ("pkgs." <> attrPath <> ".src.urls")

nixBuild :: Text -> Sh (Either Text ())
nixBuild attrPath = do
  buildE <-
    shE $
    cmd
      "nix-build"
      "--option"
      "sandbox"
      "true"
      "--option"
      "restrict-eval"
      "true"
      "-A"
      attrPath
  case buildE of
    Right _ -> return $ Right ()
    Left _ -> do
      buildLogE <-
        cmd "nix" "log" "-f" "." attrPath &
        (shE >>>
         (fmap . fmap)
           (T.lines >>> reverse >>> take 30 >>> reverse >>> T.unlines))
      return $
        case buildLogE of
          Left t -> Left "nix log failed trying to get build logs"
          Right buildLog -> Left ("nix build failed.\n" <> buildLog)
