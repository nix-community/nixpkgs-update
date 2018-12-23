{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Update
  ( updateAll
  ) where

import qualified Blacklist
import qualified Check
import Clean (fixSrcUrl)
import Control.Applicative ((<|>))
import Control.Error
import Control.Exception (SomeException, throw, toException)
import Control.Exception.Lifted
import Control.Monad (forM_, mplus)
import Control.Monad.Except
import Control.Monad.Trans.Class
import Data.Function ((&))
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import qualified File
import qualified GH
import qualified Git
import NeatInterpolation (text)
import qualified Nix
import Outpaths
import Prelude hiding (FilePath)
import Shelly.Lifted
import Utils
  ( Options(..)
  , UpdateEnv(..)
  , Version
  , branchName
  , canFail
  , eitherToError
  , ensureVersionCompatibleWithPathPin
  , orElse
  , ourShell
  , parseUpdates
  , rewriteError
  , tRead
  , shE
  )

default (T.Text)

data MergeBaseOutpathsInfo = MergeBaseOutpathsInfo
  { lastUpdated :: UTCTime
  , mergeBaseOutpaths :: Set ResultLine
  }

log' logFile msg
    -- TODO: switch to Data.Time.Format.ISO8601 once time-1.9.0 is available
 = do
  runDate <-
    T.pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) <$>
    liftIO getCurrentTime
  appendfile logFile (runDate <> " " <> msg <> "\n")

updateAll :: Options -> IO ()
updateAll options =
  ourShell options $ do
    let logFile = fromText (workingDir options) </> "ups.log"
    mkdir_p (fromText (workingDir options))
    touchfile logFile
    updates <- readfile "packages-to-update.txt"
    let log = log' logFile
    appendfile logFile "\n\n"
    log "New run of ups.sh"
    twoHoursAgo <-
      liftIO $ addUTCTime (fromInteger $ -60 * 60 * 2) <$> getCurrentTime
    mergeBaseOutpathSet <-
      liftIO $ newIORef (MergeBaseOutpathsInfo twoHoursAgo S.empty)
    updateLoop options log (parseUpdates updates) mergeBaseOutpathSet

updateLoop ::
     Options
  -> (Text -> Sh ())
  -> [Either Text (Text, Version, Version)]
  -> IORef MergeBaseOutpathsInfo
  -> Sh ()
updateLoop _ log [] _ = log "ups.sh finished"
updateLoop options log (Left e:moreUpdates) mergeBaseOutpathsContext = do
  log e
  updateLoop options log moreUpdates mergeBaseOutpathsContext
updateLoop options log (Right (package, oldVersion, newVersion):moreUpdates) mergeBaseOutpathsContext = do
  log (package <> " " <> oldVersion <> " -> " <> newVersion)
  let updateEnv = UpdateEnv package oldVersion newVersion options
  updated <- updatePackage log updateEnv mergeBaseOutpathsContext
  case updated of
    Left failure -> do
      liftIO $ Git.cleanup (branchName updateEnv)
      log $ "FAIL " <> failure
      if ".0" `T.isSuffixOf` newVersion
        then let Just newNewVersion = ".0" `T.stripSuffix` newVersion
              in updateLoop
                   options
                   log
                   (Right (package, oldVersion, newNewVersion) : moreUpdates)
                   mergeBaseOutpathsContext
        else updateLoop options log moreUpdates mergeBaseOutpathsContext
    Right _ -> do
      log "SUCCESS"
      updateLoop options log moreUpdates mergeBaseOutpathsContext

updatePackage ::
     (Text -> Sh ())
  -> UpdateEnv
  -> IORef MergeBaseOutpathsInfo
  -> Sh (Either Text ())
updatePackage log updateEnv mergeBaseOutpathsContext =
  runExceptT $ do
    Blacklist.packageName (packageName updateEnv)
    -- Check whether requested version is newer than the current one
    Nix.compareVersions updateEnv
    Git.fetchIfStale
    Git.checkAutoUpdateBranchDoesn'tExist (packageName updateEnv)
    Git.cleanAndResetToMaster
    attrPath <- Nix.lookupAttrPath updateEnv
    ensureVersionCompatibleWithPathPin updateEnv attrPath
    srcUrls <- Nix.getSrcUrls attrPath
    Blacklist.srcUrl srcUrls
    Blacklist.attrPath attrPath
    masterShowRef <- lift $ Git.showRef "master"
    lift $ log masterShowRef
    derivationFile <- Nix.getDerivationFile updateEnv attrPath
    flip catches [Handler (\(ex :: SomeException) -> throwE (T.pack (show ex)))] $
      -- Make sure it hasn't been updated on master
     do
      masterDerivationContents <- lift $ readfile derivationFile
      Nix.assertOldVersionOn updateEnv "master" masterDerivationContents
      -- Make sure it hasn't been updated on staging
      Git.cleanAndResetToStaging
      masterShowRef <- lift $ Git.showRef "staging"
      lift $ log masterShowRef
      stagingDerivationContents <- lift $ readfile derivationFile
      Nix.assertOldVersionOn updateEnv "staging" stagingDerivationContents
      lift $ Git.checkoutAtMergeBase (branchName updateEnv)
      oneHourAgo <-
        liftIO $ addUTCTime (fromInteger $ -60 * 60) <$> getCurrentTime
      mergeBaseOutpathsInfo <- liftIO $ readIORef mergeBaseOutpathsContext
      mergeBaseOutpathSet <-
        if lastUpdated mergeBaseOutpathsInfo < oneHourAgo
          then do
            mbos <- ExceptT $ currentOutpathSet
            now <- liftIO $ getCurrentTime
            liftIO $
              writeIORef
                mergeBaseOutpathsContext
                (MergeBaseOutpathsInfo now mbos)
            return $ mbos
          else return $ mergeBaseOutpaths mergeBaseOutpathsInfo
      derivationContents <- lift $ readfile derivationFile
      when
        (Nix.numberOfFetchers derivationContents > 1)
        (throwE $ "More than one fetcher in " <> toTextIgnore derivationFile)
      Blacklist.content derivationContents
      oldHash <- Nix.getOldHash attrPath
      oldSrcUrl <- Nix.getSrcUrl attrPath
      lift $
        File.replace
          (oldVersion updateEnv)
          (newVersion updateEnv)
          derivationFile
      newSrcUrl <- Nix.getSrcUrl attrPath
      when (oldSrcUrl == newSrcUrl) $ throwE "Source url did not change."
      lift $ File.replace oldHash Nix.sha256Zero derivationFile
      newHash <- Nix.getHashFromBuild (attrPath <> ".src") <|>
                 Nix.getHashFromBuild attrPath -- <|>
                 -- lift (fixSrcUrl updateEnv derivationFile attrPath oldSrcUrl) <|>
                 -- throwE "Could not get new hash."
      when (oldHash == newHash) $ throwE "Hashes equal; no update necessary"
      lift $ File.replace Nix.sha256Zero newHash derivationFile
      editedOutpathSet <- ExceptT $ currentOutpathSet
      let opDiff = S.difference mergeBaseOutpathSet editedOutpathSet
      let numPRebuilds = numPackageRebuilds opDiff
      when
        (numPRebuilds > 10 &&
         "buildPythonPackage" `T.isInfixOf` derivationContents)
        (throwE $
         "Python package with too many package rebuilds " <>
         (T.pack . show) numPRebuilds <>
         "  > 10")
      Nix.build attrPath
      result <- Nix.resultLink
      publishPackage log updateEnv oldSrcUrl newSrcUrl attrPath result opDiff

publishPackage ::
     (Text -> Sh ())
  -> UpdateEnv
  -> Text
  -> Text
  -> Text
  -> FilePath
  -> Set ResultLine
  -> ExceptT Text Sh ()
publishPackage log updateEnv oldSrcUrl newSrcUrl attrPath result opDiff = do
  lift $ log ("cachix " <> (T.pack . show) result)
  lift $ Nix.cachix result
  resultCheckReport <-
    case Blacklist.checkResult (packageName updateEnv) of
      Right () -> lift $ sub (Check.result updateEnv result)
      Left msg -> pure msg
  d <- Nix.getDescription attrPath
  let metaDescription =
        "\n\nmeta.description for " <> attrPath <> " is: '" <> d <> "'."
  releaseUrlResult <- liftIO $ GH.releaseUrl newSrcUrl
  releaseUrlMessage <-
    case releaseUrlResult of
      Left e -> do
        lift $ log e
        return ""
      Right msg -> return ("\n[Release on GitHub](" <> msg <> ")\n\n")
  compareUrlResult <- liftIO $ GH.compareUrl oldSrcUrl newSrcUrl
  compareUrlMessage <-
    case compareUrlResult of
      Left e -> do
        lift $ log e
        return "\n"
      Right msg -> return ("\n[Compare changes on GitHub](" <> msg <> ")\n\n")
  maintainers <- Nix.getMaintainers attrPath
  let maintainersCc =
        if not (T.null maintainers)
          then "\n\ncc " <> maintainers <> " for testing."
          else ""
  let commitMsg = commitMessage updateEnv attrPath
  ExceptT $ shE $ Git.commit commitMsg
  commitHash <- lift $ Git.headHash
  -- Try to push it three times
  Git.push updateEnv <|>  Git.push updateEnv <|> Git.push updateEnv
  isBroken <- Nix.getIsBroken attrPath
  lift $ untilOfBorgFree
  let base =
        if numPackageRebuilds opDiff < 100
          then "master"
          else "staging"
  lift $ GH.pr
    base
    (prMessage
       updateEnv
       isBroken
       metaDescription
       releaseUrlMessage
       compareUrlMessage
       resultCheckReport
       commitHash
       attrPath
       maintainersCc
       result
       (outpathReport opDiff))
  liftIO $ Git.cleanAndResetToMaster

repologyUrl :: UpdateEnv -> Text
repologyUrl updateEnv = [text|https://repology.org/metapackage/$pname/versions|]
  where
    pname = updateEnv & packageName & T.toLower
commitMessage :: UpdateEnv -> Text -> Text
commitMessage updateEnv attrPath =
  let oV = oldVersion updateEnv
      nV = newVersion updateEnv
      repologyLink = repologyUrl updateEnv
   in [text|
       $attrPath: $oV -> $nV

       Semi-automatic update generated by
       https://github.com/ryantm/nixpkgs-update tools. This update was made
       based on information from
       $repologyLink
     |]

brokenWarning :: Bool -> Text
brokenWarning False = ""
brokenWarning True =
  "- WARNING: Package has meta.broken=true; Please manually test this package update and remove the broken attribute."

prMessage ::
     UpdateEnv
  -> Bool
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> FilePath
  -> Text
  -> Text
prMessage updateEnv isBroken metaDescription releaseUrlMessage compareUrlMessage resultCheckReport commitHash attrPath maintainersCc resultPath opReport =
  let brokenMsg = brokenWarning isBroken
      oV = oldVersion updateEnv
      nV = newVersion updateEnv
      repologyLink = repologyUrl updateEnv
      result = toTextIgnore resultPath
   in [text|
       $attrPath: $oV -> $nV

       Semi-automatic update generated by https://github.com/ryantm/nixpkgs-update tools. This update was made based on information from $repologyLink.
       $brokenMsg
       $metaDescription
       $releaseUrlMessage
       $compareUrlMessage
       <details>
       <summary>
       Checks done (click to expand)
       </summary>

       - built on NixOS
       $resultCheckReport

       </details>
       <details>
       <summary>
       Rebuild report (if merged into master) (click to expand)
       </summary>

       $opReport

       </details>

       <details>
       <summary>
       Instructions to test this update (click to expand)
       </summary>

       Either download from Cachix:
       ```
       nix-store -r $result \
         --option binary-caches 'https://cache.nixos.org/ https://r-ryantm.cachix.org/' \
         --option trusted-public-keys '
         r-ryantm.cachix.org-1:gkUbLkouDAyvBdpBX0JOdIiD2/DP1ldF3Z3Y6Gqcc4c=
         cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
         '
       ```
       (r-ryantm's Cachix cache is only trusted for this store-path realization.)

       Or, build yourself:
       ```
       nix-build -A $attrPath https://github.com/r-ryantm/nixpkgs/archive/$commitHash.tar.gz
       ```

       After you've downloaded or built it, look at the files and if there are any, run the binaries:
       ```
       ls -la $result
       ls -la $result/bin
       ```


       </details>
       <br/>
       $maintainersCc
    |]

untilOfBorgFree :: Sh ()
untilOfBorgFree = do
  waiting :: Int <-
    tRead <$>
    canFail
      (cmd "curl" "-s" "https://events.nix.ci/stats.php" -|-
       cmd "jq" ".evaluator.messages.waiting")
  when (waiting > 2) $ do
    sleep 60
    untilOfBorgFree
