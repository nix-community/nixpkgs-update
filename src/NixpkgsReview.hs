{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module NixpkgsReview
  ( cacheDir,
    runReport,
  )
where

import Data.Maybe (fromJust)
import Data.Text as T
import qualified File as F
import Language.Haskell.TH.Env (envQ)
import OurPrelude
import Polysemy.Output (Output, output)
import qualified Process as P
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Exit (ExitCode (..))
import qualified Utils
import Prelude hiding (log)

binPath :: String
binPath = fromJust ($$(envQ "NIXPKGSREVIEW") :: Maybe String) <> "/bin"

cacheDir :: IO FilePath
cacheDir = getUserCacheDir "nixpkgs-review"

revDir :: FilePath -> Text -> FilePath
revDir cache commit = cache <> "/rev-" <> T.unpack commit

run ::
  Members '[F.File, P.Process, Output Text] r =>
  FilePath ->
  Text ->
  Sem r Text
run cache commit = do
  -- TODO: probably just skip running nixpkgs-review if the directory
  -- already exists
  void $
    ourReadProcessInterleavedSem $
      proc "rm" ["-rf", revDir cache commit]
  (exitCode, nixpkgsReviewOutput) <-
    ourReadProcessInterleavedSem $
      proc "timeout" ["45m", (binPath <> "/nixpkgs-review"), "rev", T.unpack commit, "--no-shell"]
  case exitCode of
    ExitSuccess -> F.read $ (revDir cache commit) <> "/report.md"
    ExitFailure 124 -> do
      output "[check][nixpkgs-review] took longer than 45m and timed out"
      return "nixpkgs-review took longer than 45m and timed out"
    ExitFailure code -> do
      output $ "[check][nixpkgs-review] errored with exit code " <> tshow code <> " and output " <> nixpkgsReviewOutput
      return "nixpkgs-review encountered an error. Please check the logs at https://r.ryantm.com/log/ for more information."

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
