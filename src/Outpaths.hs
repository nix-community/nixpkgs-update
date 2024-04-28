{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Outpaths
  ( currentOutpathSet,
    currentOutpathSetUncached,
    ResultLine,
    dummyOutpathSetBefore,
    dummyOutpathSetAfter,
    numPackageRebuilds,
    outpathReport,
  )
where

import Data.List (sort)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Git
import OurPrelude
import qualified System.Directory
import qualified System.Posix.Files as F
import Text.Parsec (parse)
import Text.Parser.Char
import Text.Parser.Combinators
import qualified Utils

outPathsExpr :: Text
outPathsExpr =
  [interpolate|
{ checkMeta
, path ? ./.
}:
let
  lib = import (path + "/lib");
  hydraJobs = import (path + "/pkgs/top-level/release.nix")
    # Compromise: accuracy vs. resources needed for evaluation.
    # we only evaluate one architecture per OS as we most likely catch all
    # mass-rebuilds this way.
    {
      supportedSystems = [
        "x86_64-linux"
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

outPath :: (MonadIO m) => ExceptT Text m Text
outPath = do
  cacheDir <- liftIO $ Utils.outpathCacheDir
  let outpathFile = (cacheDir </> "outpaths.nix")
  liftIO $ T.writeFile outpathFile outPathsExpr
  liftIO $ putStrLn "[outpaths] eval start"
  currentDir <- liftIO $ System.Directory.getCurrentDirectory
  result <-
    ourReadProcessInterleaved_ $
      proc
        "nix-env"
        [ "-f",
          outpathFile,
          "-qaP",
          "--no-name",
          "--out-path",
          "--arg",
          "path",
          currentDir,
          "--arg",
          "checkMeta",
          "true",
          "--show-trace"
        ]
  liftIO $ putStrLn "[outpaths] eval end"
  pure result

data Outpath = Outpath
  { mayName :: Maybe Text,
    storePath :: Text
  }
  deriving (Eq, Ord, Show)

data ResultLine = ResultLine
  { package :: Text,
    architecture :: Text,
    outpaths :: Vector Outpath
  }
  deriving (Eq, Ord, Show)

-- Example query result line:
-- testInput :: Text
-- testInput =
--   "haskellPackages.amazonka-dynamodb-streams.x86_64-linux                        doc=/nix/store/m4rpsc9nx0qcflh9ni6qdlg6hbkwpicc-amazonka-dynamodb-streams-1.6.0-doc;/nix/store/rvd4zydr22a7j5kgnmg5x6695c7bgqbk-amazonka-dynamodb-streams-1.6.0\nhaskellPackages.agum.x86_64-darwin                                            doc=/nix/store/n526rc0pa5h0krdzsdni5agcpvcd3cb9-agum-2.7-doc;/nix/store/s59r75svbjm724q5iaprq4mln5k6wcr9-agum-2.7"
currentOutpathSet :: (MonadIO m) => ExceptT Text m (Set ResultLine)
currentOutpathSet = do
  rev <- Git.headRev
  mayOp <- lift $ lookupOutPathByRev rev
  op <- case mayOp of
    Just paths -> pure paths
    Nothing -> do
      paths <- outPath
      dir <- Utils.outpathCacheDir
      let file = dir <> "/" <> T.unpack rev
      liftIO $ T.writeFile file paths
      pure paths
  parse parseResults "outpath" op & fmapL tshow & hoistEither

currentOutpathSetUncached :: (MonadIO m) => ExceptT Text m (Set ResultLine)
currentOutpathSetUncached = do
  op <- outPath
  parse parseResults "outpath" op & fmapL tshow & hoistEither

lookupOutPathByRev :: (MonadIO m) => Text -> m (Maybe Text)
lookupOutPathByRev rev = do
  dir <- Utils.outpathCacheDir
  let file = dir <> "/" <> T.unpack rev
  fileExists <- liftIO $ F.fileExist file
  case fileExists of
    False -> return Nothing
    True -> do
      paths <- liftIO $ readFile file
      return $ Just $ T.pack paths

dummyOutpathSetBefore :: Text -> Set ResultLine
dummyOutpathSetBefore attrPath = S.singleton (ResultLine attrPath "x86-64" (V.singleton (Outpath (Just "attrPath") "fakepath")))

dummyOutpathSetAfter :: Text -> Set ResultLine
dummyOutpathSetAfter attrPath = S.singleton (ResultLine attrPath "x86-64" (V.singleton (Outpath (Just "attrPath") "fakepath-edited")))

parseResults :: (CharParsing m) => m (Set ResultLine)
parseResults = S.fromList <$> parseResultLine `sepEndBy` newline

parseResultLine :: (CharParsing m) => m ResultLine
parseResultLine =
  ResultLine
    <$> (T.dropWhileEnd (== '.') <$> parseAttrpath)
    <*> parseArchitecture
    <* spaces
    <*> parseOutpaths

parseAttrpath :: (CharParsing m) => m Text
parseAttrpath = T.concat <$> many (try parseAttrpathPart)

parseAttrpathPart :: (CharParsing m) => m Text
parseAttrpathPart = T.snoc <$> (T.pack <$> many (noneOf ". ")) <*> char '.'

parseArchitecture :: (CharParsing m) => m Text
parseArchitecture = T.pack <$> many (noneOf " ")

parseOutpaths :: (CharParsing m) => m (Vector Outpath)
parseOutpaths = V.fromList <$> (parseOutpath `sepBy1` char ';')

parseOutpath :: (CharParsing m) => m Outpath
parseOutpath =
  Outpath
    <$> optional (try (T.pack <$> (many (noneOf "=\n") <* char '=')))
    <*> (T.pack <$> many (noneOf ";\n"))

packageRebuilds :: Set ResultLine -> Vector Text
packageRebuilds = S.toList >>> fmap package >>> sort >>> V.fromList >>> V.uniq

numPackageRebuilds :: Set ResultLine -> Int
numPackageRebuilds diff = V.length $ packageRebuilds diff

outpathReport :: Set ResultLine -> Text
outpathReport diff =
  let pkg = tshow $ V.length $ packageRebuilds diff
      firstFifty = T.unlines $ V.toList $ V.take 50 $ packageRebuilds diff
      numPaths = tshow $ S.size diff
   in [interpolate|
        $numPaths total rebuild path(s)

        $pkg package rebuild(s)

        First fifty rebuilds by attrpath
        $firstFifty
      |]
