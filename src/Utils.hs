{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Utils (Options(..), Version, setupNixpkgs, tRead, parseUpdates) where

import Prelude hiding (FilePath)
import Data.Text (Text)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Shelly

default (T.Text)

type Version = Text

data Options = Options {
    dryRun :: Bool
}

setupNixpkgs :: Sh FilePath
setupNixpkgs = do
    home <- get_env_text "HOME"
    let nixpkgsPath = home </> ".cache" </> "nixpkgs"

    unlessM (test_e nixpkgsPath) $ do
        cmd "hub" "clone" "nixpkgs" nixpkgsPath -- requires that user has forked nixpkgs
        cmd "cd" nixpkgsPath
        cmd "git" "remote" "add" "upstream" "https://github.com/NixOS/nixpkgs"
        cmd "git" "fetch" "upstream"
        cmd "git" "fetch" "origin" "staging"
        cmd "git" "fetch" "upstream" "staging"

    cmd "cd" nixpkgsPath

    return nixpkgsPath


parseUpdates :: Text -> [(Text, Version, Version)]
parseUpdates = map (toTriple . T.words) . T.lines where
    toTriple :: [Text] -> (Text, Version, Version)
    toTriple [package, oldVersion, newVersion] = (package, oldVersion, newVersion)
    toTriple line = error $ T.unpack ("Unable to parse update: " <> T.unwords line)

tRead ::  Read a => Text -> a
tRead = read . T.unpack
