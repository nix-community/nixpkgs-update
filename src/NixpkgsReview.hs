{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NixpkgsReview
  ( cacheDir,
    runReport,
  )
where

import Data.Text as T
import qualified File as F
import OurPrelude
import Polysemy.Output (Output, output)
import qualified Process as P
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Exit ()
import qualified Utils
import Prelude hiding (log)

cacheDir :: IO FilePath
cacheDir = getUserCacheDir "nixpkgs-review"

revDir :: FilePath -> Text -> FilePath
revDir cache commit = cache <> "/rev-" <> T.unpack commit

run ::
  Members '[F.File, P.Process, Output Text, Embed IO] r =>
  FilePath ->
  Text ->
  Sem r Text
run cache commit =
  let timeout = "180m" :: Text
   in do
        -- TODO: probably just skip running nixpkgs-review if the directory
        -- already exists
        void $
          ourReadProcessInterleavedSem $
            proc "rm" ["-rf", revDir cache commit]
        (exitCode, _nixpkgsReviewOutput) <-
          ourReadProcessInterleavedSem $
            proc "timeout" [T.unpack timeout, "nixpkgs-review", "rev", T.unpack commit, "--no-shell", "--extra-nixpkgs-config", "{ allowBroken = false; }"]
        case exitCode of
          ExitFailure 124 -> do
            output $ "[check][nixpkgs-review] took longer than " <> timeout <> " and timed out"
            return $ ":warning: nixpkgs-review took longer than " <> timeout <> " and timed out"
          _ -> do
            reportExists <- embed $ doesFileExist (revDir cache commit <> "/report.md")
            if reportExists
              then F.read $ (revDir cache commit) <> "/report.md"
              else do
                output $ "[check][nixpkgs-review] report.md does not exist"
                return $ ":x: nixpkgs-review failed"

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
      . Utils.runLog log
      $ NixpkgsReview.run c commit
  log msg
  return msg
