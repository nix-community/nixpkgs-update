{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Update
  ( updateAll
  ) where

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
  )
import NeatInterpolation (text)
import Nix
  ( Raw(..)
  , compareVersions
  , getDerivationFile
  , getIsBroken
  , getMaintainers
  , getOldHash
  , getSrcUrl
  , getSrcUrls
  , lookupAttrPath
  , nixEvalE
  , nixBuild
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

nameBlackList :: [(Text -> Bool, Text)]
nameBlackList =
  [ (("jquery" `T.isInfixOf`), "this isn't a real package")
  , (("google-cloud-sdk" `T.isInfixOf`), "complicated package")
  , (("github-release" `T.isInfixOf`), "complicated package")
  , (("fcitx" `T.isInfixOf`), "gets stuck in daemons")
  , ( ("libxc" `T.isInfixOf`)
    , "currently people don't want to update this https://github.com/NixOS/nixpkgs/pull/35821")
  , (("perl" `T.isInfixOf`), "currently don't know how to update perl")
  , (("python" `T.isInfixOf`), "currently don't know how to update python")
  , (("cdrtools" `T.isInfixOf`), "We keep downgrading this by accident.")
  , (("gst" `T.isInfixOf`), "gstreamer plugins are kept in lockstep.")
  , (("electron" `T.isInfixOf`), "multi-platform srcs in file.")
  , ( ("linux-headers" `T.isInfixOf`)
    , "Not updated until many packages depend on it (part of stdenv).")
  , ( ("mpich" `T.isInfixOf`)
    , "Reported on repology.org as mischaracterized newest version")
  , (("xfce" `T.isInfixOf`), "@volth asked to not update xfce")
  , (("cmake-cursesUI-qt4UI" `T.isInfixOf`), "Derivation file is complicated")
  , ( ("varnish" `T.isInfixOf`)
    , "Temporary blacklist because of multiple versions and slow nixpkgs update")
  , (("iana-etc" `T.isInfixOf`), "@mic92 takes care of this package")
  , ( ("checkbashism" `T.isInfixOf`)
    , "needs to be fixed, see https://github.com/NixOS/nixpkgs/pull/39552")
  , ((== "isl"), "multi-version long building package")
  , ((== "tokei"), "got stuck forever building with no CPU usage")
  , ( ("qscintilla" `T.isInfixOf`)
    , "https://github.com/ryantm/nixpkgs-update/issues/51")
  ]

contentBlacklist :: [(Text, Text)]
contentBlacklist =
  [ ("DO NOT EDIT", "Derivation file says not to edit it.")
    -- Skip packages that have special builders
  , ("buildGoPackage", "Derivation contains buildGoPackage.")
  , ("buildRustCrate", "Derivation contains buildRustCrate.")
  , ("buildPythonPackage", "Derivation contains buildPythonPackage.")
  , ("buildRubyGem", "Derivation contains buildRubyGem.")
  , ("bundlerEnv", "Derivation contains bundlerEnv.")
  , ("buildPerlPackage", "Derivation contains buildPerlPackage.")
  ]

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
  setupNixpkgs
  let errorExit = errorExit' log (branchName updateEnv)
  -- Check whether requested version is newer than the current one
  eitherToError errorExit (compareVersions updateEnv)
  -- Check whether package name is on blacklist
  forM_ nameBlackList $ \(isBlacklisted, message) ->
    when (isBlacklisted (packageName updateEnv)) $ errorExit message
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
    forM_ contentBlacklist $ \(offendingContent, message) ->
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
    resultCheckReport <- sub (checkResult updateEnv result)
    maintainers <- eitherToError errorExit (getMaintainers attrPath)
    let maintainersCc =
          if not (T.null maintainers)
            then "\n\ncc " <> maintainers <> " for testing"
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
    -- Try to push it three times
    push updateEnv `orElse` push updateEnv `orElse` push updateEnv
    isBroken <- eitherToError errorExit (getIsBroken attrPath)
    let brokenWarning =
          if isBroken == "true"
            then "- WARNING: Package has meta.broken=true; Please manually test this package update and remove the broken attribute."
            else ""
    let prMessage = commitMessage <> brokenWarning <> maintainersCc
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
