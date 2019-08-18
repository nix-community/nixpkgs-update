{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Update
  ( updateAll
  ) where

import OurPrelude

import qualified Blacklist
import qualified Check
import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.IORef
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified File
import qualified GH
import qualified Git
import qualified Nix
import Outpaths
import Prelude hiding (log)
import qualified Time
import Utils
  ( Options(..)
  , UpdateEnv(..)
  , Version
  , branchName
  , parseUpdates
  , prTitle
  , runtimeDir
  )
import qualified Version

default (T.Text)

data MergeBaseOutpathsInfo =
  MergeBaseOutpathsInfo
    { lastUpdated :: UTCTime
    , mergeBaseOutpaths :: Set ResultLine
    }

log' :: MonadIO m => FilePath -> Text -> m ()
log' logFile msg = do
  runDate <- liftIO $ runM $ Time.runIO Time.runDate
  liftIO $ T.appendFile logFile (runDate <> " " <> msg <> "\n")

updateAll :: Options -> Text -> IO ()
updateAll o updates = do
  rDir <- runtimeDir
  let logFile = rDir <> "/ups.log"
  let log = log' logFile
  T.appendFile logFile "\n\n"
  log "New run of ups.sh"
  twoHoursAgo <- runM $ Time.runIO Time.twoHoursAgo
  mergeBaseOutpathSet <-
    liftIO $ newIORef (MergeBaseOutpathsInfo twoHoursAgo S.empty)
  updateLoop o log (parseUpdates updates) mergeBaseOutpathSet

updateLoop ::
     MonadIO m
  => Options
  -> (Text -> m ())
  -> [Either Text (Text, Version, Version)]
  -> IORef MergeBaseOutpathsInfo
  -> m ()
updateLoop _ log [] _ = log "ups.sh finished"
updateLoop o log (Left e:moreUpdates) mergeBaseOutpathsContext = do
  log e
  updateLoop o log moreUpdates mergeBaseOutpathsContext
updateLoop o log (Right (pName, oldVer, newVer):moreUpdates) mergeBaseOutpathsContext = do
  log (pName <> " " <> oldVer <> " -> " <> newVer)
  let updateEnv = UpdateEnv pName oldVer newVer o
  updated <- updatePackage log updateEnv mergeBaseOutpathsContext
  case updated of
    Left failure -> do
      log $ "FAIL " <> failure
      cleanupResult <- runExceptT $ Git.cleanup (branchName updateEnv)
      case cleanupResult of
        Left e -> liftIO $ print e
        _ ->
          if ".0" `T.isSuffixOf` newVer
            then let Just newNewVersion = ".0" `T.stripSuffix` newVer
                  in updateLoop
                       o
                       log
                       (Right (pName, oldVer, newNewVersion) : moreUpdates)
                       mergeBaseOutpathsContext
            else updateLoop o log moreUpdates mergeBaseOutpathsContext
    Right _ -> do
      log "SUCCESS"
      updateLoop o log moreUpdates mergeBaseOutpathsContext

-- Arguments this function should have to make it testable:
-- * the merge base commit (should be updated externally to this function)
-- * the merge base context should be updated externally to this function
-- * the commit for branches: master, staging, staging-next, python-unstable
updatePackage ::
     MonadIO m
  => (Text -> m ())
  -> UpdateEnv
  -> IORef MergeBaseOutpathsInfo
  -> m (Either Text ())
updatePackage log updateEnv mergeBaseOutpathsContext =
  runExceptT $ do
    Blacklist.packageName (packageName updateEnv)
    Nix.assertNewerVersion updateEnv
    Git.fetchIfStale <|> liftIO (T.putStrLn "Failed to fetch.")
    Git.checkAutoUpdateBranchDoesntExist (packageName updateEnv)
    Git.cleanAndResetTo "master"
    attrPath <- Nix.lookupAttrPath updateEnv
    GH.checkExistingUpdatePR updateEnv attrPath
    Blacklist.attrPath attrPath
    Version.assertCompatibleWithPathPin updateEnv attrPath
    srcUrls <- Nix.getSrcUrls attrPath
    Blacklist.srcUrl srcUrls
    derivationFile <- Nix.getDerivationFile attrPath
    assertNotUpdatedOn updateEnv derivationFile "master"
    assertNotUpdatedOn updateEnv derivationFile "staging"
    assertNotUpdatedOn updateEnv derivationFile "staging-next"
    assertNotUpdatedOn updateEnv derivationFile "python-unstable"
    Git.checkoutAtMergeBase (branchName updateEnv)
    oneHourAgo <- liftIO $ runM $ Time.runIO Time.oneHourAgo
    mergeBaseOutpathsInfo <- liftIO $ readIORef mergeBaseOutpathsContext
    mergeBaseOutpathSet <-
      if lastUpdated mergeBaseOutpathsInfo < oneHourAgo
        then do
          mbos <- currentOutpathSet
          now <- liftIO getCurrentTime
          liftIO $
            writeIORef mergeBaseOutpathsContext (MergeBaseOutpathsInfo now mbos)
          return mbos
        else return $ mergeBaseOutpaths mergeBaseOutpathsInfo
    derivationContents <- liftIO $ T.readFile derivationFile
    Nix.assertOneOrFewerFetcher derivationContents derivationFile
    Blacklist.content derivationContents
    oldHash <- Nix.getOldHash attrPath
    oldSrcUrl <- Nix.getSrcUrl attrPath
    lift $
      File.replace (oldVersion updateEnv) (newVersion updateEnv) derivationFile
    newSrcUrl <- Nix.getSrcUrl attrPath
    when (oldSrcUrl == newSrcUrl) $ throwE "Source url did not change. "
    lift $ File.replace oldHash Nix.sha256Zero derivationFile
    newHash <- Nix.getHashFromBuild attrPath -- <|>
               -- lift (fixSrcUrl updateEnv derivationFile attrPath oldSrcUrl) <|>
               -- throwE "Could not get new hash. "
    tryAssert "Hashes equal; no update necessary" (oldHash /= newHash)
    lift $ File.replace Nix.sha256Zero newHash derivationFile
    editedOutpathSet <- currentOutpathSet
    let opDiff = S.difference mergeBaseOutpathSet editedOutpathSet
    let numPRebuilds = numPackageRebuilds opDiff
    Blacklist.python numPRebuilds derivationContents
    when (numPRebuilds == 0) (throwE "Update edits cause no rebuilds.")
    Nix.build attrPath
    result <- Nix.resultLink
    publishPackage log updateEnv oldSrcUrl newSrcUrl attrPath result opDiff

publishPackage ::
     MonadIO m
  => (Text -> m ())
  -> UpdateEnv
  -> Text
  -> Text
  -> Text
  -> Text
  -> Set ResultLine
  -> ExceptT Text m ()
publishPackage log updateEnv oldSrcUrl newSrcUrl attrPath result opDiff = do
  lift $ log ("cachix " <> (T.pack . show) result)
  Nix.cachix result
  resultCheckReport <-
    case Blacklist.checkResult (packageName updateEnv) of
      Right () -> lift $ Check.result updateEnv (T.unpack result)
      Left msg -> pure msg
  d <- Nix.getDescription attrPath <|> return T.empty
  u <- Nix.getHomepage attrPath <|> return T.empty
  let metaDescription =
        if d == T.empty
          then ""
          else "\n\nmeta.description for " <> attrPath <> " is: '" <> d <> "'."
  let metaHomepage =
        if u == T.empty
          then ""
          else "\n\nmeta.homepage for " <> attrPath <> " is: '" <> u
  releaseUrlMessage <-
    (do msg <- GH.releaseUrl newSrcUrl
        return ("\n[Release on GitHub](" <> msg <> ")\n\n")) <|>
    return ""
  compareUrlMessage <-
    (do msg <- GH.compareUrl oldSrcUrl newSrcUrl
        return ("\n[Compare changes on GitHub](" <> msg <> ")\n\n")) <|>
    return "\n"
  maintainers <- Nix.getMaintainers attrPath
  let maintainersCc =
        if not (T.null maintainers)
          then "\n\ncc " <> maintainers <> " for testing."
          else ""
  let commitMsg = commitMessage updateEnv attrPath
  Git.commit commitMsg
  commitHash <- Git.headHash
  -- Try to push it three times
  Git.push updateEnv <|> Git.push updateEnv <|> Git.push updateEnv
  isBroken <- Nix.getIsBroken attrPath
  lift untilOfBorgFree
  let base =
        if numPackageRebuilds opDiff < 100
          then "master"
          else "staging"
  lift $
    GH.pr
      base
      (prMessage
         updateEnv
         isBroken
         metaDescription
         metaHomepage
         releaseUrlMessage
         compareUrlMessage
         resultCheckReport
         commitHash
         attrPath
         maintainersCc
         result
         (outpathReport opDiff))
  Git.cleanAndResetTo "master"

repologyUrl :: UpdateEnv -> Text
repologyUrl updateEnv =
  [interpolate|https://repology.org/metapackage/$pname/versions|]
  where
    pname = updateEnv & packageName & T.toLower

commitMessage :: UpdateEnv -> Text -> Text
commitMessage updateEnv attrPath =
  let title = prTitle updateEnv attrPath
      repologyLink = repologyUrl updateEnv
   in [interpolate|
       $title

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
  -> Text
  -> Text
  -> Text
  -> Text
prMessage updateEnv isBroken metaDescription metaHomepage releaseUrlMessage compareUrlMessage resultCheckReport commitHash attrPath maintainersCc resultPath opReport =
  let brokenMsg = brokenWarning isBroken
      title = prTitle updateEnv attrPath
      repologyLink = repologyUrl updateEnv
   in [interpolate|
       $title

       Semi-automatic update generated by https://github.com/ryantm/nixpkgs-update tools. This update was made based on information from $repologyLink.
       $brokenMsg
       $metaDescription
       $metaHomepage
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
       nix-store -r $resultPath \
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
       ls -la $resultPath
       ls -la $resultPath/bin
       ```


       </details>
       <br/>
       $maintainersCc
    |]

untilOfBorgFree :: MonadIO m => m ()
untilOfBorgFree = do
  stats <-
    shell "curl -s https://events.nix.ci/stats.php" & readProcessInterleaved_
  waiting <-
    shell "jq .evaluator.messages.waiting" & setStdin (byteStringInput stats) &
    readProcessInterleaved_ &
    fmap (BSL.readInt >>> fmap fst >>> fromMaybe 0)
  when (waiting > 2) $ do
    liftIO $ threadDelay 60000000
    untilOfBorgFree

assertNotUpdatedOn ::
     MonadIO m => UpdateEnv -> FilePath -> Text -> ExceptT Text m ()
assertNotUpdatedOn updateEnv derivationFile branch = do
  Git.cleanAndResetTo branch
  derivationContents <- fmapLT tshow $ tryIO $ T.readFile derivationFile
  Nix.assertOldVersionOn updateEnv branch derivationContents
