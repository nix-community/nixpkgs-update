{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Outpaths where

import Control.Category ((>>>))
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.List (sort)
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified NeatInterpolation (text)
import Shelly
import Text.Parsec (parse)
import Text.Parser.Char
import Text.Parser.Combinators
import Utils

default (Text)

outPathsExpr =
  [NeatInterpolation.text|

(let
  lib = import ./lib;
  hydraJobs = import ./pkgs/top-level/release.nix
    # Compromise: accuracy vs. resources needed for evaluation.
    {
      supportedSystems = [
        "aarch64-linux"
        "i686-linux"
        "x86_64-linux"
        "x86_64-darwin"
      ];

      nixpkgsArgs = {
        config = {
          allowBroken = true;
          allowUnfree = true;
          allowInsecurePredicate = x: true;
          checkMeta = true;

          handleEvalIssue = reason: errormsg:
            if reason == "unknown-meta"
              then abort errormsg
              else true;

          inHydra = true;
        };
      };
    };
  recurseIntoAttrs = attrs: attrs // { recurseForDerivations = true; };

  # hydraJobs leaves recurseForDerivations as empty attrmaps;
  # that would break nix-env and we also need to recurse everywhere.
  tweak = lib.mapAttrs
    (name: val:
      if name == "recurseForDerivations" then true
      else if lib.isAttrs val && val.type or null != "derivation"
              then recurseIntoAttrs (tweak val)
      else val
    );

  # Some of these contain explicit references to platform(s) we want to avoid;
  # some even (transitively) depend on ~/.nixpkgs/config.nix (!)
  blacklist = [
    "tarball" "metrics" "manual"
    "darwin-tested" "unstable" "stdenvBootstrapTools"
    "moduleSystem" "lib-tests" # these just confuse the output
  ];

in
  tweak (builtins.removeAttrs hydraJobs blacklist))
|]

--downloadOutPath :: Sh ()
outPath :: Sh Text
outPath =
  sub $ do
    cmd
      "curl"
      "-o"
      "outpaths.nix"
      "https://raw.githubusercontent.com/NixOS/ofborg/released/ofborg/src/outpaths.nix"
    setenv "GC_INITIAL_HEAP_SIZE" "10g"
    cmd
      "nix-env"
      "-f"
      "./outpaths.nix"
      "-qaP"
      "--no-name"
      "--out-path"
      "--arg"
      "checkMeta"
      "true"

data Outpath = Outpath
  { mayName :: Maybe Text
  , storePath :: Text
  } deriving (Eq, Ord, Show)

data ResultLine = ResultLine
  { package :: Text
  , architecture :: Text
  , outpaths :: Vector Outpath
  } deriving (Eq, Ord, Show)

-- Example query result line:
testInput =
  "haskellPackages.amazonka-dynamodb-streams.x86_64-linux                        doc=/nix/store/m4rpsc9nx0qcflh9ni6qdlg6hbkwpicc-amazonka-dynamodb-streams-1.6.0-doc;/nix/store/rvd4zydr22a7j5kgnmg5x6695c7bgqbk-amazonka-dynamodb-streams-1.6.0\nhaskellPackages.agum.x86_64-darwin                                            doc=/nix/store/n526rc0pa5h0krdzsdni5agcpvcd3cb9-agum-2.7-doc;/nix/store/s59r75svbjm724q5iaprq4mln5k6wcr9-agum-2.7"

currentOutpathSet :: Sh (Either Text (Set ResultLine))
currentOutpathSet =
  first (show >>> T.pack) . parse parseResults "outpath" <$> silently outPath

parseResults :: CharParsing m => m (Set ResultLine)
parseResults = S.fromList <$> parseResultLine `sepEndBy` newline

parseResultLine :: CharParsing m => m ResultLine
parseResultLine =
  ResultLine <$> (T.dropWhileEnd (== '.') <$> parseAttrpath) <*>
  parseArchitecture <*
  spaces <*>
  parseOutpaths

parseAttrpath :: CharParsing m => m Text
parseAttrpath = T.concat <$> many (try parseAttrpathPart)

parseAttrpathPart :: CharParsing m => m Text
parseAttrpathPart = T.append <$> (T.pack <$> many (noneOf ". ")) <*> text "."

parseArchitecture :: CharParsing m => m Text
parseArchitecture = T.pack <$> many (noneOf " ")

parseOutpaths :: CharParsing m => m (Vector Outpath)
parseOutpaths = V.fromList <$> (parseOutpath `sepBy1` text ";")

parseOutpath :: CharParsing m => m Outpath
parseOutpath =
  Outpath <$> optional (try (T.pack <$> (many (noneOf "=\n") <* text "="))) <*>
  (T.pack <$> many (noneOf ";\n"))

packageRebuilds :: Set ResultLine -> Vector Text
packageRebuilds = S.toList >>> fmap package >>> sort >>> V.fromList >>> V.uniq

numPackageRebuilds :: Set ResultLine -> Int
numPackageRebuilds diff = V.length $ packageRebuilds diff

archRebuilds :: Text -> Set ResultLine -> Int
archRebuilds arch =
  S.toList >>> fmap architecture >>> filter (== arch) >>> length

darwinRebuilds :: Set ResultLine -> Int
darwinRebuilds = archRebuilds "x86_64-darwin"

linuxRebuilds :: Set ResultLine -> Int
linuxRebuilds = archRebuilds "x86_64-linux"

linux32bRebuilds :: Set ResultLine -> Int
linux32bRebuilds = archRebuilds "i686-linux"

armRebuilds :: Set ResultLine -> Int
armRebuilds = archRebuilds "aarch64-linux"

outpathReport :: Set ResultLine -> Text
outpathReport diff =
  let tshow = show >>> T.pack
      pkg = tshow $ V.length $ packageRebuilds diff
      firstFifty = T.unlines $ V.toList $ V.take 50 $ packageRebuilds diff
      darwin = tshow $ darwinRebuilds diff
      linux = tshow $ linuxRebuilds diff
      linux32b = tshow $ linux32bRebuilds diff
      arm = tshow $ armRebuilds diff
      numPaths = tshow $ S.size diff
   in [NeatInterpolation.text|
        $numPaths total rebuild path(s)

        $pkg package rebuild(s)

        $linux x86_64-linux rebuild(s)
        $linux32b i686-linux rebuild(s)
        $darwin x86_64-darwin rebuild(s)
        $arm aarch64-linux rebuild(s)


        First fifty rebuilds by attrpath
        $firstFifty
      |]
