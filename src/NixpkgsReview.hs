{-# LANGUAGE OverloadedStrings #-}

module NixpkgsReview
  ( cacheDir,
    runReport,
  )
where

import Data.Text as T
import qualified File as F
import OurPrelude
import qualified Process as P
import System.Environment.XDG.BaseDir (getUserCacheDir)
import Prelude hiding (log)

cacheDir :: IO FilePath
cacheDir = getUserCacheDir "nixpkgs-review"

revDir :: FilePath -> Text -> FilePath
revDir cache commit = cache <> "/rev-" <> T.unpack commit

run ::
  Members '[F.File, P.Process] r =>
  FilePath ->
  Text ->
  Sem r Text
run cache commit = do
  -- TODO: probably just skip running nixpkgs-review if the directory
  -- already exists
  void $ ourReadProcessInterleavedSem $
    proc "rm" ["-rf", revDir cache commit]
  void $ ourReadProcessInterleavedSem $
    proc "nixpkgs-review" ["rev", T.unpack commit, "--no-shell"]
  F.read $ (revDir cache commit) <> "/report.md"

-- Assumes we are already in nixpkgs dir
runReport :: (Text -> IO ()) -> Text -> IO Text
runReport log commit = do
  log "[check][nixpkgs-review]"
  c <- cacheDir
  msg <-
    runFinal
      . embedToFinal
      . F.runIO
      . P.runIO
      $ NixpkgsReview.run c commit
  log msg
  return msg
