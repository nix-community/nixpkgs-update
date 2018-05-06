{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Nix
  ( nixEvalE
  , compareVersions
  , lookupAttrPath
  ) where

import Control.Category ((>>>))
import Control.Error (headMay)
import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Shelly (Sh, cmd, run)
import Utils (UpdateEnv(..), rewriteError, shE)

type Raw = Bool

nixEvalE :: Raw -> Text -> Sh (Either Text Text)
nixEvalE raw expr =
  run
    "nix"
    (["eval", "-f", "."] <>
     if raw
       then ["--raw"]
       else [] <> [expr]) &
  (fmap T.strip >>> shE >>> rewriteError ("nix eval failed for " <> expr))

-- Error if the "new version" is actually newer according to nix
compareVersions :: UpdateEnv -> Sh (Either Text ())
compareVersions updateEnv = do
  versionComparison <-
    nixEvalE
      False
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
