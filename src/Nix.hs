{-# LANGUAGE OverloadedStrings #-}

module Nix
  ( nixEvalE
  , compareVersions
  ) where

import           Control.Category ((>>>))
import           Data.Bifunctor (second)
import           Data.Function ((&))
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Shelly (Sh, run)
import           Utils (UpdateEnv(..), shE, rewriteError)

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
  return $ case versionComparison of
    Right "1" -> Right ()
    Right a -> Left $
      newVersion updateEnv <> " is not newer than " <> oldVersion updateEnv <>
      " according to Nix; versionComparison: " <> a
    Left a -> Left a
