{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Outpaths
where

import           Data.Semigroup ((<>))
import           Data.Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified NeatInterpolation (text)
import           Shelly
import qualified Text.Parsec (parse)
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Utils

default (Text)

outPathsExpr = [NeatInterpolation.text|

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
outPath = sub $ do
  Utils.setupNixpkgs
  cmd "curl" "-o" "outpaths.nix" "https://raw.githubusercontent.com/NixOS/ofborg/released/ofborg/src/outpaths.nix"
  setenv "GC_INITIAL_HEAP_SIZE" "10g"
  cmd "nix-env" "-f" "./outpaths.nix" "-qaP" "--no-name" "--out-path" "--arg" "checkMeta" "true"

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
-- haskellPackages.agum.x86_64-darwin                                            doc=/nix/store/n526rc0pa5h0krdzsdni5agcpvcd3cb9-agum-2.7-doc;/nix/store/s59r75svbjm724q5iaprq4mln5k6wcr9-agum-2.7
parse :: CharParsing m => m (Set ResultLine)
parse = undefined

parseResultLine :: CharParsing m =>  m ResultLine
parseResultLine =
  ResultLine <$>
  parseAttrpath <*>
  parseArchitecture <*
  spaces <*>
  parseOutpaths

parseAttrpath :: CharParsing m => m Text
parseAttrpath =
  T.concat <$> many (try parseAttrpathPart)

parseAttrpathPart :: CharParsing m => m Text
parseAttrpathPart =
  T.append <$>
  (T.pack <$> many (noneOf ". ")) <*>
  text "."

parseArchitecture :: CharParsing m => m Text
parseArchitecture = T.pack <$> many (noneOf " ")

parseOutpaths :: CharParsing m => m (Vector Outpath)
parseOutpaths = V.fromList <$> (parseOutpath `sepBy1` text ";")

parseOutpath :: CharParsing m => m Outpath
parseOutpath =
  Outpath <$>
  optional (try (T.pack <$> (many (noneOf "=") <* text "="))) <*>
  (T.pack <$> many (noneOf ";"))
