{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Update
  ( addPatched,
    assertNotUpdatedOn,
    cveAll,
    cveReport,
    prMessage,
    sourceGithubAll,
    updateAll,
    updatePackage,
  )
where

import CVE (CVE, cveID, cveLI)
import qualified Check
import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar (showGregorian)
import Data.Time.Clock (UTCTime, getCurrentTime, utctDay)
import qualified GH
import qualified Git
import Language.Haskell.TH.Env (envQ)
import NVD (getCVEs, withVulnDB)
import qualified Nix
import qualified NixpkgsReview
import OurPrelude
import Outpaths
import qualified Rewrite
import qualified Skiplist
import qualified Time
import Utils
  ( Options (..),
    URL,
    UpdateEnv (..),
    Version,
    branchName,
    logDir,
    parseUpdates,
    prTitle,
    whenBatch,
  )
import qualified Version
import Prelude hiding (log)

default (T.Text)

data MergeBaseOutpathsInfo = MergeBaseOutpathsInfo
  { lastUpdated :: UTCTime,
    mergeBaseOutpaths :: Set ResultLine
  }

log' :: MonadIO m => FilePath -> Text -> m ()
log' logFile msg = do
  runDate <- liftIO $ runM $ Time.runIO Time.runDate
  liftIO $ T.appendFile logFile (runDate <> " " <> msg <> "\n")

logFileName :: IO String
logFileName = do
  lDir <- logDir
  now <- getCurrentTime
  let logFile = lDir <> "/" <> showGregorian (utctDay now) <> ".log"
  putStrLn ("Using log file: " <> logFile)
  return logFile

getLog :: Options -> IO (Text -> IO ())
getLog o = do
  if batchUpdate o
    then do
      logFile <- logFileName
      let log = log' logFile
      T.appendFile logFile "\n\n"
      return log
    else return T.putStrLn

notifyOptions :: (Text -> IO ()) -> Options -> IO ()
notifyOptions log o = do
  let repr f = if f o then "YES" else "NO"
  let ghUser = GH.untagName . githubUser $ o
  let pr = repr doPR
  let outpaths = repr calculateOutpaths
  let cve = repr makeCVEReport
  let review = repr runNixpkgsReview
  npDir <- tshow <$> Git.nixpkgsDir
  log $
    [interpolate|
    Configured Nixpkgs-Update Options:
    ----------------------------------
    GitHub User:                   $ghUser
    Send pull request on success:  $pr
    Calculate Outpaths:            $outpaths
    CVE Security Report:           $cve
    Run nixpkgs-review:            $review
    Nixpkgs Dir:                   $npDir
    ----------------------------------|]

updateAll :: Options -> Text -> IO ()
updateAll o updates = do
  log <- getLog o
  log "New run of nixpkgs-update"
  notifyOptions log o
  twoHoursAgo <- runM $ Time.runIO Time.twoHoursAgo
  mergeBaseOutpathSet <-
    liftIO $ newIORef (MergeBaseOutpathsInfo twoHoursAgo S.empty)
  updateLoop o log (parseUpdates updates) mergeBaseOutpathSet

cveAll :: Options -> Text -> IO ()
cveAll o updates = do
  let u' = rights $ parseUpdates updates
  results <-
    mapM
      ( \(p, oldV, newV, url) -> do
          r <- cveReport (UpdateEnv p oldV newV url o)
          return $ p <> ": " <> oldV <> " -> " <> newV <> "\n" <> r
      )
      u'
  T.putStrLn (T.unlines results)

sourceGithubAll :: Options -> Text -> IO ()
sourceGithubAll o updates = do
  let u' = rights $ parseUpdates updates
  _ <-
    runExceptT $ do
      Git.fetchIfStale <|> liftIO (T.putStrLn "Failed to fetch.")
      Git.cleanAndResetTo "master"
  mapM_
    ( \(p, oldV, newV, url) -> do
        let updateEnv = UpdateEnv p oldV newV url o
        runExceptT $ do
          attrPath <- Nix.lookupAttrPath updateEnv
          srcUrl <- Nix.getSrcUrl attrPath
          v <- GH.latestVersion updateEnv srcUrl
          if v /= newV
            then
              liftIO $
                T.putStrLn $
                  p <> ": " <> oldV <> " -> " <> newV <> " -> " <> v
            else return ()
    )
    u'

updateLoop ::
  Options ->
  (Text -> IO ()) ->
  [Either Text (Text, Version, Version, Maybe URL)] ->
  IORef MergeBaseOutpathsInfo ->
  IO ()
updateLoop _ log [] _ = log "nixpkgs-update finished"
updateLoop o log (Left e : moreUpdates) mergeBaseOutpathsContext = do
  log e
  updateLoop o log moreUpdates mergeBaseOutpathsContext
updateLoop o log (Right (pName, oldVer, newVer, url) : moreUpdates) mergeBaseOutpathsContext = do
  log (pName <> " " <> oldVer <> " -> " <> newVer <> fromMaybe "" (fmap (" " <>) url))
  let updateEnv = UpdateEnv pName oldVer newVer url o
  updated <- updatePackageBatch log updateEnv mergeBaseOutpathsContext
  case updated of
    Left failure -> do
      log $ "FAIL " <> failure
      cleanupResult <- runExceptT $ Git.cleanup (branchName updateEnv)
      case cleanupResult of
        Left e -> liftIO $ print e
        _ ->
          if ".0" `T.isSuffixOf` newVer
            then
              let Just newNewVersion = ".0" `T.stripSuffix` newVer
               in updateLoop
                    o
                    log
                    (Right (pName, oldVer, newNewVersion, url) : moreUpdates)
                    mergeBaseOutpathsContext
            else updateLoop o log moreUpdates mergeBaseOutpathsContext
    Right _ -> do
      log "SUCCESS"
      updateLoop o log moreUpdates mergeBaseOutpathsContext

-- Arguments this function should have to make it testable:
-- - the merge base commit (should be updated externally to this function)
-- - the merge base context should be updated externally to this function
-- - the commit for branches: master, staging, staging-next
updatePackageBatch ::
  (Text -> IO ()) ->
  UpdateEnv ->
  IORef MergeBaseOutpathsInfo ->
  IO (Either Text ())
updatePackageBatch log updateEnv@UpdateEnv {..} mergeBaseOutpathsContext =
  runExceptT $ do
    let pr = doPR options

    -- Filters that don't need git
    whenBatch updateEnv do
      Skiplist.packageName packageName
      -- Update our git checkout
      Git.fetchIfStale <|> liftIO (T.putStrLn "Failed to fetch.")
      Git.cleanAndResetTo "master"

    -- Filters: various cases where we shouldn't update the package
    attrPath <- Nix.lookupAttrPath updateEnv
    hasUpdateScript <- Nix.hasUpdateScript attrPath

    whenBatch updateEnv do
      Skiplist.attrPath attrPath
      when pr do
        Git.checkAutoUpdateBranchDoesntExist packageName
        GH.checkExistingUpdatePR updateEnv attrPath

    unless hasUpdateScript do
      Nix.assertNewerVersion updateEnv
      Version.assertCompatibleWithPathPin updateEnv attrPath

    derivationFile <- Nix.getDerivationFile attrPath
    unless hasUpdateScript do
      assertNotUpdatedOn updateEnv derivationFile "master"
      assertNotUpdatedOn updateEnv derivationFile "staging"
      assertNotUpdatedOn updateEnv derivationFile "staging-next"

    -- Calculate output paths for rebuilds and our merge base
    mergeBase <- if batchUpdate options
      then Git.checkoutAtMergeBase (branchName updateEnv)
      else pure "HEAD"
    let calcOutpaths = calculateOutpaths options
    oneHourAgo <- liftIO $ runM $ Time.runIO Time.oneHourAgo
    mergeBaseOutpathsInfo <- liftIO $ readIORef mergeBaseOutpathsContext
    mergeBaseOutpathSet <-
      if calcOutpaths && lastUpdated mergeBaseOutpathsInfo < oneHourAgo
        then do
          mbos <- currentOutpathSet
          now <- liftIO getCurrentTime
          liftIO $
            writeIORef mergeBaseOutpathsContext (MergeBaseOutpathsInfo now mbos)
          return mbos
        else
          if calcOutpaths
            then return $ mergeBaseOutpaths mergeBaseOutpathsInfo
            else return $ dummyOutpathSetBefore attrPath

    -- Get the original values for diffing purposes
    derivationContents <- liftIO $ T.readFile derivationFile
    oldHash <- Nix.getOldHash attrPath
    oldSrcUrl <- Nix.getSrcUrl attrPath
    oldVerMay <- rightMay `fmapRT` (lift $ runExceptT $ Nix.getAttr Nix.Raw "version" attrPath)

    tryAssert
      "The derivation has no 'version' attribute, so do not know how to figure out the version while doing an updateScript update"
      (not hasUpdateScript || isJust oldVerMay)

    -- One final filter
    Skiplist.content derivationContents

    ----------------------------------------------------------------------------
    -- UPDATES
    --
    -- At this point, we've stashed the old derivation contents and
    -- validated that we actually should be rewriting something. Get
    -- to work processing the various rewrite functions!
    rewriteMsgs <- Rewrite.runAll log Rewrite.Args {..}
    ----------------------------------------------------------------------------

    -- Compute the diff and get updated values
    diffAfterRewrites <- Git.diff mergeBase
    tryAssert
      "The diff was empty after rewrites."
      (diffAfterRewrites /= T.empty)
    lift . log $ "Diff after rewrites:\n" <> diffAfterRewrites
    updatedDerivationContents <- liftIO $ T.readFile derivationFile
    newSrcUrl <- Nix.getSrcUrl attrPath
    newHash <- Nix.getHash attrPath
    newVerMay <- rightMay `fmapRT` (lift $ runExceptT $ Nix.getAttr Nix.Raw "version" attrPath)

    tryAssert
      "The derivation has no 'version' attribute, so do not know how to figure out the version while doing an updateScript update"
      (not hasUpdateScript || isJust newVerMay)

    -- Sanity checks to make sure the PR is worth opening
    unless hasUpdateScript do
      when (derivationContents == updatedDerivationContents) $ throwE "No rewrites performed on derivation."
      when (oldSrcUrl == newSrcUrl) $ throwE "Source url did not change. "
      when (oldHash == newHash) $ throwE "Hashes equal; no update necessary"
    editedOutpathSet <- if calcOutpaths then currentOutpathSet else return $ dummyOutpathSetAfter attrPath
    let opDiff = S.difference mergeBaseOutpathSet editedOutpathSet
    let numPRebuilds = numPackageRebuilds opDiff
    whenBatch updateEnv do
      Skiplist.python numPRebuilds derivationContents
    when (numPRebuilds == 0) (throwE "Update edits cause no rebuilds.")
    Nix.build attrPath
    --
    -- Update updateEnv if using updateScript
    updateEnv' <-
      if hasUpdateScript
        then do
          -- Already checked that these are Just above.
          let Just oldVer = oldVerMay
          let Just newVer = newVerMay
          return $
            UpdateEnv
              packageName
              oldVer
              newVer
              (Just "passthru.updateScript")
              options
        else return updateEnv

    --
    -- Publish the result
    lift . log $ "Successfully finished processing"
    result <- Nix.resultLink
    publishPackage log updateEnv' oldSrcUrl newSrcUrl attrPath result (Just opDiff) rewriteMsgs
    whenBatch updateEnv do
      Git.cleanAndResetTo "master"

publishPackage ::
  (Text -> IO ()) ->
  UpdateEnv ->
  Text ->
  Text ->
  Text ->
  Text ->
  Maybe (Set ResultLine) ->
  [Text] ->
  ExceptT Text IO ()
publishPackage log updateEnv oldSrcUrl newSrcUrl attrPath result opDiff rewriteMsgs = do
  let prBase =
        if (isNothing opDiff || numPackageRebuilds (fromJust opDiff) < 100)
          then "master"
          else "staging"
  cachixTestInstructions <- doCachix log updateEnv result
  resultCheckReport <-
    case Skiplist.checkResult (packageName updateEnv) of
      Right () -> lift $ Check.result updateEnv (T.unpack result)
      Left msg -> pure msg
  metaDescription <- Nix.getDescription attrPath <|> return T.empty
  metaHomepage <- Nix.getHomepageET attrPath <|> return T.empty
  metaChangelog <- Nix.getChangelog attrPath <|> return T.empty
  cveRep <- liftIO $ cveReport updateEnv
  releaseUrl <- GH.releaseUrl updateEnv newSrcUrl <|> return ""
  compareUrl <- GH.compareUrl oldSrcUrl newSrcUrl <|> return ""
  maintainers <- Nix.getMaintainers attrPath
  let commitMsg = commitMessage updateEnv attrPath
  Git.commit commitMsg
  commitHash <- Git.headHash
  nixpkgsReviewMsg <-
    if prBase /= "staging" && (runNixpkgsReview . options $ updateEnv)
      then liftIO $ NixpkgsReview.runReport log commitHash
      else return ""
  -- Try to push it three times
  when
    (doPR . options $ updateEnv)
    (Git.push updateEnv <|> Git.push updateEnv <|> Git.push updateEnv)
  isBroken <- Nix.getIsBroken attrPath
  when
    (batchUpdate . options $ updateEnv)
    (lift untilOfBorgFree)
  let prMsg =
        prMessage
          updateEnv
          isBroken
          metaDescription
          metaHomepage
          metaChangelog
          rewriteMsgs
          releaseUrl
          compareUrl
          resultCheckReport
          commitHash
          attrPath
          maintainers
          result
          (fromMaybe "" (outpathReport <$> opDiff))
          cveRep
          cachixTestInstructions
          nixpkgsReviewMsg
  liftIO $ log prMsg
  if (doPR . options $ updateEnv)
    then do
      let ghUser = GH.untagName . githubUser . options $ updateEnv
      pullRequestUrl <- GH.pr updateEnv (prTitle updateEnv attrPath) prMsg (ghUser <> ":" <> (branchName updateEnv)) prBase
      liftIO $ log pullRequestUrl
    else liftIO $ T.putStrLn prMsg

commitMessage :: UpdateEnv -> Text -> Text
commitMessage updateEnv attrPath = prTitle updateEnv attrPath

brokenWarning :: Bool -> Text
brokenWarning False = ""
brokenWarning True =
  "- WARNING: Package has meta.broken=true; Please manually test this package update and remove the broken attribute."

prMessage ::
  UpdateEnv ->
  Bool ->
  Text ->
  Text ->
  Text ->
  [Text] ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text
prMessage updateEnv isBroken metaDescription metaHomepage metaChangelog rewriteMsgs releaseUrl compareUrl resultCheckReport commitHash attrPath maintainers resultPath opReport cveRep cachixTestInstructions nixpkgsReviewMsg =
  -- Some components of the PR description are pre-generated prior to calling
  -- because they require IO, but in general try to put as much as possible for
  -- the formatting into the pure function so that we can control the body
  -- formatting in one place and unit test it.
  let brokenMsg = brokenWarning isBroken
      metaHomepageLine =
        if metaHomepage == T.empty
          then ""
          else "meta.homepage for " <> attrPath <> " is: " <> metaHomepage
      metaDescriptionLine =
        if metaDescription == T.empty
          then ""
          else "meta.description for " <> attrPath <> " is: " <> metaDescription
      metaChangelogLine =
        if metaDescription == T.empty
          then ""
          else "meta.changelog for " <> attrPath <> " is: " <> metaChangelog
      rewriteMsgsLine = foldl (\ms m -> ms <> T.pack "\n- " <> m) "\n###### Updates performed" rewriteMsgs
      maintainersCc =
        if not (T.null maintainers)
          then "cc " <> maintainers <> " for [testing and approval](https://github.com/Mic92/nixpkgs-review#readme)."
          else ""
      releaseUrlMessage =
        if releaseUrl == T.empty
          then ""
          else "- [Release on GitHub](" <> releaseUrl <> ")"
      compareUrlMessage =
        if compareUrl == T.empty
          then ""
          else "- [Compare changes on GitHub](" <> compareUrl <> ")"
      nixpkgsReviewSection =
        if nixpkgsReviewMsg == T.empty
          then "NixPkgs review skipped"
          else
            [interpolate|
            We have automatically built all packages that will get rebuilt due to
            this change.

            This gives evidence on whether the upgrade will break dependent packages.
            Note sometimes packages show up as _failed to build_ independent of the
            change, simply because they are already broken on the target branch.

            $nixpkgsReviewMsg
            |]
      pat link = [interpolate|This update was made based on information from $link.|]
      sourceLinkInfo = maybe "" pat $ sourceURL updateEnv
      ghUser = GH.untagName . githubUser . options $ updateEnv
      batch = batchUpdate . options $ updateEnv
      automatic = if batch then "Automatic" else "Semi-automatic"
   in [interpolate|
       $automatic update generated by [nixpkgs-update](https://github.com/ryantm/nixpkgs-update) tools. $sourceLinkInfo
       $brokenMsg

       $metaDescriptionLine

       $metaHomepageLine

       $metaChangelogLine

       $rewriteMsgsLine

       ###### To inspect upstream changes

       $releaseUrlMessage

       $compareUrlMessage

       ###### Impact

       <details>
       <summary>
       <b>Checks done</b> (click to expand)
       </summary>

       ---

       - built on NixOS
       $resultCheckReport

       ---

       </details>
       <details>
       <summary>
       <b>Rebuild report</b> (if merged into master) (click to expand)
       </summary>

       ```
       $opReport
       ```

       </details>

       <details>
       <summary>
       <b>Instructions to test this update</b> (click to expand)
       </summary>

       ---

       $cachixTestInstructions
       ```
       nix-build -A $attrPath https://github.com/$ghUser/nixpkgs/archive/$commitHash.tar.gz
       ```

       After you've downloaded or built it, look at the files and if there are any, run the binaries:
       ```
       ls -la $resultPath
       ls -la $resultPath/bin
       ```

       ---

       </details>
       <br/>

       $cveRep

       ### Pre-merge build results

       $nixpkgsReviewSection

       ---

       ###### Maintainer pings

       $maintainersCc
    |]

jqBin :: String
jqBin = fromJust ($$(envQ "JQ") :: Maybe String) <> "/bin/jq"

untilOfBorgFree :: MonadIO m => m ()
untilOfBorgFree = do
  stats <-
    shell "curl -s https://events.nix.ci/stats.php" & readProcessInterleaved_
  waiting <-
    shell (jqBin <> " .evaluator.messages.waiting") & setStdin (byteStringInput stats)
      & readProcessInterleaved_
      & fmap (BSL.readInt >>> fmap fst >>> fromMaybe 0)
  when (waiting > 2) $ do
    liftIO $ threadDelay 60000000
    untilOfBorgFree

assertNotUpdatedOn ::
  MonadIO m => UpdateEnv -> FilePath -> Text -> ExceptT Text m ()
assertNotUpdatedOn updateEnv derivationFile branch = do
  npDir <- liftIO $ Git.nixpkgsDir
  let Just file = T.stripPrefix (T.pack npDir <> "/") (T.pack derivationFile)
  derivationContents <- Git.show branch file
  Nix.assertOldVersionOn updateEnv branch derivationContents

addPatched :: Text -> Set CVE -> IO [(CVE, Bool)]
addPatched attrPath set = do
  let list = S.toList set
  forM
    list
    ( \cve -> do
        patched <- runExceptT $ Nix.hasPatchNamed attrPath (cveID cve)
        let p =
              case patched of
                Left _ -> False
                Right r -> r
        return (cve, p)
    )

cveReport :: UpdateEnv -> IO Text
cveReport updateEnv =
  if not (makeCVEReport . options $ updateEnv)
    then return ""
    else withVulnDB $ \conn -> do
      let pname1 = packageName updateEnv
      let pname2 = T.replace "-" "_" pname1
      oldCVEs1 <- getCVEs conn pname1 (oldVersion updateEnv)
      oldCVEs2 <- getCVEs conn pname2 (oldVersion updateEnv)
      let oldCVEs = S.fromList (oldCVEs1 ++ oldCVEs2)
      newCVEs1 <- getCVEs conn pname1 (newVersion updateEnv)
      newCVEs2 <- getCVEs conn pname2 (newVersion updateEnv)
      let newCVEs = S.fromList (newCVEs1 ++ newCVEs2)
      let inOldButNotNew = S.difference oldCVEs newCVEs
          inNewButNotOld = S.difference newCVEs oldCVEs
          inBoth = S.intersection oldCVEs newCVEs
          ifEmptyNone t =
            if t == T.empty
              then "none"
              else t
      inOldButNotNew' <- addPatched (packageName updateEnv) inOldButNotNew
      inNewButNotOld' <- addPatched (packageName updateEnv) inNewButNotOld
      inBoth' <- addPatched (packageName updateEnv) inBoth
      let toMkdownList = fmap (uncurry cveLI) >>> T.unlines >>> ifEmptyNone
          fixedList = toMkdownList inOldButNotNew'
          newList = toMkdownList inNewButNotOld'
          unresolvedList = toMkdownList inBoth'
      if fixedList == "none" && unresolvedList == "none" && newList == "none"
        then return ""
        else
          return
            [interpolate|
      ###### Security vulnerability report

      <details>
      <summary>
      Security report (click to expand)
      </summary>

      CVEs resolved by this update:
      $fixedList

      CVEs introduced by this update:
      $newList

      CVEs present in both versions:
      $unresolvedList


       </details>
       <br/>
      |]

doCachix :: MonadIO m => (Text -> m ()) -> UpdateEnv -> Text -> ExceptT Text m Text
doCachix log updateEnv resultPath =
  let o = options updateEnv
  in
    if batchUpdate o && "r-ryantm" == (GH.untagName $ githubUser o)
    then do
      return
        [interpolate|
       Either **download from Cachix**:
       ```
       nix-store -r $resultPath \
         --option binary-caches 'https://cache.nixos.org/ https://nix-community.cachix.org/' \
         --option trusted-public-keys '
         nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=
         cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
         '
       ```
       (The Cachix cache is only trusted for this store-path realization.)
       For the Cachix download to work, your user must be in the `trusted-users` list or you can use `sudo` since root is effectively trusted.

       Or, **build yourself**:
       |]
    else do
      lift $ log "skipping cachix"
      return "Build yourself:"

updatePackage ::
  Options ->
  Text ->
  IO (Either Text ())
updatePackage o updateInfo = do
  let (p, oldV, newV, url) = head (rights (parseUpdates updateInfo))
  let updateEnv = UpdateEnv p oldV newV url o
  let log = T.putStrLn
  liftIO $ notifyOptions log o
  twoHoursAgo <- runM $ Time.runIO Time.twoHoursAgo
  mergeBaseOutpathSet <-
    liftIO $ newIORef (MergeBaseOutpathsInfo twoHoursAgo S.empty)
  updatePackageBatch log updateEnv mergeBaseOutpathSet
