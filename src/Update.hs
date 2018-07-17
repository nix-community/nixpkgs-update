{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Update
  ( updateAll
  ) where

import qualified Blacklist
import qualified Check
import Clean (fixSrcUrl)
import Control.Category ((>>>))
import Control.Exception (SomeException, throw, toException)
import Control.Monad (forM_)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import qualified File
import qualified Git
import qualified GitHub
import NeatInterpolation (text)
import qualified Nix
import Prelude hiding (FilePath)
import Shelly
import Utils
  ( ExitCode(..)
  , Options(..)
  , UpdateEnv(..)
  , Version
  , branchName
  , canFail
  , checkAttrPathVersion
  , eitherToError
  , orElse
  , ourShell
  , parseUpdates
  , rewriteError
  , setupNixpkgs
  , shE
  , tRead
  )

default (T.Text)

errorExit' :: (Text -> Sh ()) -> Text -> Text -> Sh a
errorExit' log branchName message = do
  Git.cleanup branchName
  log message
  throw (ExitCode 1)

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
    updateLoop options log (parseUpdates updates)

updateLoop ::
     Options
  -> (Text -> Sh ())
  -> [Either Text (Text, Version, Version)]
  -> Sh ()
updateLoop _ log [] = log "ups.sh finished"
updateLoop options log (Left e:moreUpdates) = do
  log e
  updateLoop options log moreUpdates
updateLoop options log (Right (package, oldVersion, newVersion):moreUpdates) = do
  log (package <> " " <> oldVersion <> " -> " <> newVersion)
  updated <-
    catch_sh
      (updatePackage log (UpdateEnv package oldVersion newVersion options))
      (\case
         ExitCode 0 -> return True
         ExitCode _ -> return False)
  if updated
    then log "SUCCESS"
    else do
      log "FAIL"
      if ".0" `T.isSuffixOf` newVersion
        then let Just newNewVersion = ".0" `T.stripSuffix` newVersion
              in updateLoop
                   options
                   log
                   (Right (package, oldVersion, newNewVersion) : moreUpdates)
        else updateLoop options log moreUpdates
  updateLoop options log moreUpdates

updatePackage :: (Text -> Sh ()) -> UpdateEnv -> Sh Bool
updatePackage log updateEnv = do
  let errorExit = errorExit' log (branchName updateEnv)
  eitherToError errorExit (pure (Blacklist.packageName (packageName updateEnv)))
  setupNixpkgs
  -- Check whether requested version is newer than the current one
  eitherToError errorExit (Nix.compareVersions updateEnv)
  -- Check whether package name is on blacklist
  Git.fetchIfStale
  whenM
    (Git.autoUpdateBranchExists (packageName updateEnv))
    (errorExit "Update branch already on origin.")
  Git.cleanAndResetToMaster
  attrPath <- eitherToError errorExit (Nix.lookupAttrPath updateEnv)
  srcUrls <- eitherToError errorExit (Nix.getSrcUrls attrPath)
  eitherToError errorExit (pure (Blacklist.srcUrl srcUrls))
  eitherToError errorExit (pure (Blacklist.attrPath attrPath))
  derivationFile <-
    eitherToError errorExit (Nix.getDerivationFile updateEnv attrPath)
  flip
    catches_sh
    [ ShellyHandler (\(ex :: ExitCode) -> throw ex)
    , ShellyHandler (\(ex :: SomeException) -> errorExit (T.pack (show ex)))
    ] $ do
    numberOfFetchers :: Int <-
      tRead <$>
      canFail
        (cmd
           "grep"
           "-Ec"
           "fetchurl {|fetchgit {|fetchFromGitHub {"
           derivationFile)
    unless (numberOfFetchers <= 1) $
      errorExit $ "More than one fetcher in " <> toTextIgnore derivationFile
    derivationContents <- readfile derivationFile
    eitherToError errorExit (pure (Blacklist.content derivationContents))
    unless (checkAttrPathVersion attrPath (newVersion updateEnv)) $
      errorExit
        ("Version in attr path " <> attrPath <> " not compatible with " <>
         newVersion updateEnv)
    -- Make sure it hasn't been updated on master
    cmd "grep" (oldVersion updateEnv) derivationFile `orElse`
      errorExit "Old version not present in master derivation file."
    -- Make sure it hasn't been updated on staging
    Git.cleanAndResetToStaging
    cmd "grep" (oldVersion updateEnv) derivationFile `orElse`
      errorExit "Old version not present in staging derivation file."
    Git.checkoutAtMergeBase (branchName updateEnv)
    oldHash <- eitherToError errorExit (Nix.getOldHash attrPath)
    oldSrcUrl <- eitherToError errorExit (Nix.getSrcUrl attrPath)
    File.replace (oldVersion updateEnv) (newVersion updateEnv) derivationFile
    newSrcUrl <- eitherToError errorExit (Nix.getSrcUrl attrPath)
    when (oldSrcUrl == newSrcUrl) $ errorExit "Source url did not change."
    newHash <-
      canFail (T.strip <$> cmd "nix-prefetch-url" "-A" (attrPath <> ".src")) `orElse`
      fixSrcUrl updateEnv derivationFile attrPath oldSrcUrl `orElse`
      errorExit "Could not prefetch new version URL."
    when (oldHash == newHash) $ errorExit "Hashes equal; no update necessary"
    File.replace oldHash newHash derivationFile
    eitherToError errorExit (Nix.build attrPath)
    result <-
      fromText <$>
      (T.strip <$>
       (cmd "readlink" "./result" `orElse` cmd "readlink" "./result-bin")) `orElse`
      errorExit "Could not find result link."
    publishPackage log updateEnv oldSrcUrl newSrcUrl attrPath result

publishPackage ::
     (Text -> Sh ()) -> UpdateEnv -> Text -> Text -> Text -> FilePath -> Sh Bool
publishPackage log updateEnv oldSrcUrl newSrcUrl attrPath result = do
  let errorExit = errorExit' log (branchName updateEnv)
  log ("cachix " <> (T.pack . show) result)
  Nix.cachix result
  resultCheckReport <-
    case Blacklist.checkResult (packageName updateEnv) of
      Right () -> sub (Check.result updateEnv result)
      Left msg -> pure msg
  d <- eitherToError errorExit (Nix.getDescription attrPath)
  let metaDescription =
        "\n\nmeta.description for " <> attrPath <> " is: '" <> d <> "'."
  releaseUrlResult <- liftIO $ GitHub.releaseUrl newSrcUrl
  releaseUrlMessage <-
    case releaseUrlResult of
      Left e -> do
        log e
        return ""
      Right msg -> return ("\n[Release on GitHub](" <> msg <> ")\n\n")
  compareUrlResult <- liftIO $ GitHub.compareUrl oldSrcUrl newSrcUrl
  compareUrlMessage <-
    case compareUrlResult of
      Left e -> do
        log e
        return "\n"
      Right msg -> return ("\n[Compare changes on GitHub](" <> msg <> ")\n\n")
  maintainers <- eitherToError errorExit (Nix.getMaintainers attrPath)
  let maintainersCc =
        if not (T.null maintainers)
          then "\n\ncc " <> maintainers <> " for testing."
          else ""
  let commitMsg = commitMessage updateEnv attrPath
  Git.commit commitMsg
  commitHash <- Git.headHash
  -- Try to push it three times
  Git.push updateEnv `orElse` Git.push updateEnv `orElse` Git.push updateEnv
  isBroken <- eitherToError errorExit (Nix.getIsBroken attrPath)
  untilOfBorgFree
  GitHub.pr
    (prMessage
       commitMsg
       isBroken
       metaDescription
       releaseUrlMessage
       compareUrlMessage
       resultCheckReport
       commitHash
       attrPath
       maintainersCc)
  Git.cleanAndResetToMaster
  return True

commitMessage :: UpdateEnv -> Text -> Text
commitMessage updateEnv attrPath =
  let oV = oldVersion updateEnv
      nV = newVersion updateEnv
      pN = packageName updateEnv
   in [text|
       $attrPath: $oV -> $nV

       Semi-automatic update generated by https://github.com/ryantm/nixpkgs-update tools. This update was made based on information from https://repology.org/metapackage/$pN/versions.
     |]

brokenWarning :: Bool -> Text
brokenWarning False = ""
brokenWarning True =
  "- WARNING: Package has meta.broken=true; Please manually test this package update and remove the broken attribute."

prMessage ::
     Text
  -> Bool
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
prMessage commitMsg isBroken metaDescription releaseUrlMessage compareUrlMessage resultCheckReport commitHash attrPath maintainersCc =
  let brokenMsg = brokenWarning isBroken
   in [text|
       $commitMsg
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
       Test this build with Cachix (click to expand)
       </summary>

       One time setup in nixpkgs Git checkout:
       ```
       cachix use r-ryantm
       git remote add r-ryantm https://github.com/r-ryantm/nixpkgs.git
       ```

       Test this build:
       ```
       git fetch r-ryantm && git checkout $commitHash && nix-shell --pure -I nixpkgs=. -p $attrPath
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
