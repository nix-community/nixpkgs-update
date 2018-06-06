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
import Check (checkResult)
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
import Git
  ( autoUpdateBranchExists
  , checkoutAtMergeBase
  , cleanAndResetToMaster
  , cleanAndResetToStaging
  , cleanup
  , commit
  , fetchIfStale
  , pr
  , push
  , headHash
  )
import NeatInterpolation (text)
import Nix
  ( Raw(..)
  , compareVersions
  , getDerivationFile
  , getDescription
  , getIsBroken
  , getMaintainers
  , getOldHash
  , getSrcUrl
  , getSrcUrls
  , lookupAttrPath
  , nixBuild
  , nixEvalE
  , cachix
  )
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
  , parseUpdates
  , rewriteError
  , setupNixpkgs
  , shE
  , tRead
  )

default (T.Text)

errorExit' :: (Text -> Sh ()) -> Text -> Text -> Sh a
errorExit' log branchName message = do
  cleanup branchName
  log message
  throw (ExitCode 1)

log' logFile msg
    -- TODO: switch to Data.Time.Format.ISO8601 once time-1.9.0 is available
 = do
  runDate <-
    T.pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) <$>
    liftIO getCurrentTime
  appendfile logFile (runDate <> " " <> msg <> "\n")

updateAll :: Options -> Sh ()
updateAll options = do
  let logFile = workingDir options </> "ups.log"
  mkdir_p (workingDir options)
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
    else log "FAIL"
  updateLoop options log moreUpdates

updatePackage :: (Text -> Sh ()) -> UpdateEnv -> Sh Bool
updatePackage log updateEnv = do
  let errorExit = errorExit' log (branchName updateEnv)
  case Blacklist.packageName (packageName updateEnv) of
    Nothing -> return ()
    Just msg -> errorExit msg
  setupNixpkgs
  -- Check whether requested version is newer than the current one
  eitherToError errorExit (compareVersions updateEnv)
  -- Check whether package name is on blacklist
  fetchIfStale
  whenM
    (autoUpdateBranchExists (packageName updateEnv))
    (errorExit "Update branch already on origin.")
  cleanAndResetToMaster
  attrPath <- eitherToError errorExit (lookupAttrPath updateEnv)
  -- Temporarily blacklist gnome sources for lockstep update
  whenM
    (("gnome" `T.isInfixOf`) <$> eitherToError errorExit (getSrcUrls attrPath))
    (errorExit "Packages from gnome are currently blacklisted.")
    -- Temporarily blacklist lua packages at @teto's request
    -- https://github.com/NixOS/nixpkgs/pull/37501#issuecomment-375169646
  when (T.isPrefixOf "lua" attrPath) $
    errorExit "Packages for lua are currently blacklisted."
  derivationFile <-
    eitherToError errorExit (getDerivationFile updateEnv attrPath)
  flip
    catches_sh
    [ ShellyHandler (\(ex :: ExitCode) -> throw ex)
    , ShellyHandler (\(ex :: SomeException) -> errorExit (T.pack (show ex)))
    ] $ do
    numberOfFetchers <-
      tRead <$>
      canFail
        (cmd
           "grep"
           "-Ec"
           "fetchurl {|fetchgit {|fetchFromGitHub {"
           derivationFile)
    unless ((numberOfFetchers :: Int) <= 1) $
      errorExit $ "More than one fetcher in " <> toTextIgnore derivationFile
    derivationContents <- readfile derivationFile
    forM_ Blacklist.content $ \(offendingContent, message) ->
      when (offendingContent `T.isInfixOf` derivationContents) $
      errorExit message
    unless (checkAttrPathVersion attrPath (newVersion updateEnv)) $
      errorExit
        ("Version in attr path " <> attrPath <> " not compatible with " <>
         newVersion updateEnv)
    -- Make sure it hasn't been updated on master
    cmd "grep" (oldVersion updateEnv) derivationFile `orElse`
      errorExit "Old version not present in master derivation file."
    -- Make sure it hasn't been updated on staging
    cleanAndResetToStaging
    cmd "grep" (oldVersion updateEnv) derivationFile `orElse`
      errorExit "Old version not present in staging derivation file."
    checkoutAtMergeBase (branchName updateEnv)
    oldHash <- eitherToError errorExit (getOldHash attrPath)
    oldSrcUrl <- eitherToError errorExit (getSrcUrl attrPath)
    File.replace (oldVersion updateEnv) (newVersion updateEnv) derivationFile
    newSrcUrl <- eitherToError errorExit (getSrcUrl attrPath)
    when (oldSrcUrl == newSrcUrl) $ errorExit "Source url did not change."
    newHash <-
      canFail (T.strip <$> cmd "nix-prefetch-url" "-A" (attrPath <> ".src")) `orElse`
      fixSrcUrl updateEnv derivationFile attrPath oldSrcUrl `orElse`
      errorExit "Could not prefetch new version URL."
    when (oldHash == newHash) $ errorExit "Hashes equal; no update necessary"
    File.replace oldHash newHash derivationFile
    eitherToError errorExit (nixBuild attrPath)
    result <-
      fromText <$>
      (T.strip <$>
       (cmd "readlink" "./result" `orElse` cmd "readlink" "./result-bin")) `orElse`
      errorExit "Could not find result link."
    cachix result
    resultCheckReport <-
      case Blacklist.checkResult (packageName updateEnv) of
        Nothing -> sub (checkResult updateEnv result)
        Just msg -> pure msg
    d <- eitherToError errorExit (getDescription attrPath)
    let metaDescription =
          "\n\nmeta.description for " <> attrPath <> " is: '" <> d <> "'."
    maintainers <- eitherToError errorExit (getMaintainers attrPath)
    let maintainersCc =
          if not (T.null maintainers)
            then "\n\ncc " <> maintainers <> " for testing."
            else ""
    let oV = oldVersion updateEnv
        nV = newVersion updateEnv
        pN = packageName updateEnv
        commitMessage =
          [text|
                $attrPath: $oV -> $nV

                Semi-automatic update generated by https://github.com/ryantm/nixpkgs-update tools.

                This update was made based on information from https://repology.org/metapackage/$pN/versions.

                These checks were done:

                - built on NixOS
                $resultCheckReport
            |]
    commit commitMessage
    commitHash <- headHash
    -- Try to push it three times
    push updateEnv `orElse` push updateEnv `orElse` push updateEnv
    isBroken <- eitherToError errorExit (getIsBroken attrPath)
    let brokenWarning =
          if isBroken == "true"
            then "- WARNING: Package has meta.broken=true; Please manually test this package update and remove the broken attribute."
            else ""

    let cachixNote =
          [text|

               **Experimental:** this build is cached with [Cachix]( https://cachix.org/ ). To use the cache follow these experimental instructions:

               One time setup in nixpkgs Git checkout:
               ```
               cachix use r-ryantm
               git remote add r-ryantm https://github.com/r-ryantm/nixpkgs.git
               ```

               Test this build:
               ```
               git fetch r-ryantm && git checkout $commitHash && nix-shell --pure -I nixpkgs=. -p $attrPath
               ```
            |]

    let prMessage =
          commitMessage <> brokenWarning <> metaDescription <> maintainersCc <> "\n\n" <> cachixNote
    untilOfBorgFree
    pr prMessage
    cleanAndResetToMaster
    return True

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
