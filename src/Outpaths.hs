{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Outpaths
  ( currentOutpathSet
  , ResultLine
  , numPackageRebuilds
  , outpathReport
  ) where

import OurPrelude

import Data.List (sort)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Text.Parsec (parse)
import Text.Parser.Char
import Text.Parser.Combinators

outPathsExpr :: Text
outPathsExpr =
  [interpolate|
#!/usr/bin/env nix-shell
# When using as a callable script, passing `--argstr path some/path` overrides $$PWD.
#!nix-shell -p nix -i "nix-env -qaP --no-name --out-path --arg checkMeta true --argstr path $$PWD -f"
{ checkMeta
, path ? ./.
}:
let
  lib = import (path + "/lib");
  hydraJobs = import (path + "/pkgs/top-level/release.nix")
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
          checkMeta = checkMeta;

          handleEvalIssue = reason: errormsg:
            let
              fatalErrors = [
                "unknown-meta" "broken-outputs"
              ];
            in if builtins.elem reason fatalErrors
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
  tweak (builtins.removeAttrs hydraJobs blacklist)
|]

outPath :: MonadIO m => ExceptT Text m Text
outPath = do
  liftIO $ putStrLn "Writing outpaths.nix..."
  liftIO $ T.writeFile "./outpaths.nix" outPathsExpr
  liftIO $ putStrLn "Evaluating outpaths..."
  ourReadProcessInterleaved_
    "nix-env -f ./outpaths.nix -qaP --no-name --out-path --arg checkMeta true --show-trace"

data Outpath =
  Outpath
    { mayName :: Maybe Text
    , storePath :: Text
    }
  deriving (Eq, Ord, Show)

data ResultLine =
  ResultLine
    { package :: Text
    , architecture :: Text
    , outpaths :: Vector Outpath
    }
  deriving (Eq, Ord, Show)

-- Example query result line:
-- testInput :: Text
-- testInput =
--   "haskellPackages.amazonka-dynamodb-streams.x86_64-linux                        doc=/nix/store/m4rpsc9nx0qcflh9ni6qdlg6hbkwpicc-amazonka-dynamodb-streams-1.6.0-doc;/nix/store/rvd4zydr22a7j5kgnmg5x6695c7bgqbk-amazonka-dynamodb-streams-1.6.0\nhaskellPackages.agum.x86_64-darwin                                            doc=/nix/store/n526rc0pa5h0krdzsdni5agcpvcd3cb9-agum-2.7-doc;/nix/store/s59r75svbjm724q5iaprq4mln5k6wcr9-agum-2.7"
currentOutpathSet :: MonadIO m => ExceptT Text m (Set ResultLine)
currentOutpathSet = do
  op <- outPath
  parse parseResults "outpath" op & fmapL tshow & hoistEither

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
parseAttrpathPart = T.snoc <$> (T.pack <$> many (noneOf ". ")) <*> char '.'

parseArchitecture :: CharParsing m => m Text
parseArchitecture = T.pack <$> many (noneOf " ")

parseOutpaths :: CharParsing m => m (Vector Outpath)
parseOutpaths = V.fromList <$> (parseOutpath `sepBy1` char ';')

parseOutpath :: CharParsing m => m Outpath
parseOutpath =
  Outpath <$> optional (try (T.pack <$> (many (noneOf "=\n") <* char '='))) <*>
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
  let pkg = tshow $ V.length $ packageRebuilds diff
      firstFifty = T.unlines $ V.toList $ V.take 50 $ packageRebuilds diff
      darwin = tshow $ darwinRebuilds diff
      linux = tshow $ linuxRebuilds diff
      linux32b = tshow $ linux32bRebuilds diff
      arm = tshow $ armRebuilds diff
      numPaths = tshow $ S.size diff
   in [interpolate|
        $numPaths total rebuild path(s)

        $pkg package rebuild(s)

        $linux x86_64-linux rebuild(s)
        $linux32b i686-linux rebuild(s)
        $darwin x86_64-darwin rebuild(s)
        $arm aarch64-linux rebuild(s)


        First fifty rebuilds by attrpath
        $firstFifty
      |]
