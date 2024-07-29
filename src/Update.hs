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
    updatePackage,
  )
where

import CVE (CVE, cveID, cveLI)
import qualified Check
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.Writer (execWriterT, tell)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromJust)
import Data.Monoid (Alt (..))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar (showGregorian)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime, utctDay)
import qualified GH
import qualified Git
import Language.Haskell.TH.Env (envQ)
import NVD (getCVEs, withVulnDB)
import qualified Nix
import qualified NixpkgsReview
import OurPrelude
import qualified Outpaths
import qualified Rewrite
import qualified Skiplist
import System.Directory (doesDirectoryExist, withCurrentDirectory)
import System.Posix.Directory (createDirectory)
import Utils
  ( Boundary (..),
    Options (..),
    UpdateEnv (..),
    VersionMatcher (..),
    branchName,
    logDir,
    parseUpdates,
    prTitle,
    whenBatch,
  )
import qualified Utils as U
import qualified Version
import Prelude hiding (log)

default (T.Text)

alsoLogToAttrPath :: Text -> (Text -> IO ()) -> IO (Text -> IO ())
alsoLogToAttrPath attrPath topLevelLog = do
  logFile <- attrPathLogFilePath attrPath
  let attrPathLog = log' logFile
  return \text -> do
    topLevelLog text
    attrPathLog text

log' :: (MonadIO m) => FilePath -> Text -> m ()
log' logFile msg = liftIO $ T.appendFile logFile (msg <> "\n")

attrPathLogFilePath :: Text -> IO String
attrPathLogFilePath attrPath = do
  lDir <- logDir
  now <- getCurrentTime
  let dir = lDir <> "/" <> T.unpack attrPath
  dirExists <- doesDirectoryExist dir
  unless
    dirExists
    (createDirectory dir U.regDirMode)
  let logFile = dir <> "/" <> showGregorian (utctDay now) <> ".log"
  putStrLn ("For attrpath " <> T.unpack attrPath <> ", using log file: " <> logFile)
  return logFile

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
  let batch = repr batchUpdate
  let outpaths = repr calculateOutpaths
  let cve = repr makeCVEReport
  let review = repr runNixpkgsReview
  let exactAttrPath = repr U.attrpath
  npDir <- tshow <$> Git.nixpkgsDir
  log $
    [interpolate| [options] github_user: $ghUser, pull_request: $pr, batch_update: $batch, calculate_outpaths: $outpaths, cve_report: $cve, nixpkgs-review: $review, nixpkgs_dir: $npDir, use attrpath: $exactAttrPath|]

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

data UpdatePackageResult = UpdatePackageSuccess | UpdatePackageFailure

-- Arguments this function should have to make it testable:
-- - the merge base commit (should be updated externally to this function)
-- - the commit for branches: master, staging, staging-next
updatePackageBatch ::
  (Text -> IO ()) ->
  Text ->
  UpdateEnv ->
  IO UpdatePackageResult
updatePackageBatch simpleLog updateInfoLine updateEnv@UpdateEnv {..} = do
  eitherFailureOrAttrpath <- runExceptT $ do
    -- Filters that don't need git
    whenBatch updateEnv do
      Skiplist.packageName packageName
      -- Update our git checkout
      Git.fetchIfStale <|> liftIO (T.putStrLn "Failed to fetch.")

    -- Filters: various cases where we shouldn't update the package
    if attrpath options
      then return packageName
      else Nix.lookupAttrPath updateEnv

  case eitherFailureOrAttrpath of
    Left failure -> do
      simpleLog failure
      return UpdatePackageFailure
    Right foundAttrPath -> do
      log <- alsoLogToAttrPath foundAttrPath simpleLog
      log updateInfoLine
      mergeBase <-
        if batchUpdate options
          then Git.mergeBase
          else pure "HEAD"
      withWorktree mergeBase foundAttrPath updateEnv $
        updateAttrPath log mergeBase updateEnv foundAttrPath

checkExistingUpdate ::
  (Text -> IO ()) ->
  UpdateEnv ->
  Maybe Text ->
  Text ->
  ExceptT Text IO ()
checkExistingUpdate log updateEnv existingCommitMsg attrPath = do
  case existingCommitMsg of
    Nothing -> lift $ log "No auto update branch exists"
    Just msg -> do
      let nV = newVersion updateEnv
      lift $
        log
          [interpolate|An auto update branch exists with message `$msg`. New version is $nV.|]

      case U.titleVersion msg of
        Just branchV
          | Version.matchVersion (RangeMatcher (Including nV) Unbounded) branchV ->
              throwError "An auto update branch exists with an equal or greater version"
        _ ->
          lift $ log "The auto update branch does not match or exceed the new version."

  -- Note that this check looks for PRs with the same old and new
  -- version numbers, so it won't stop us from updating an existing PR
  -- if this run updates the package to a newer version.
  GH.checkExistingUpdatePR updateEnv attrPath

updateAttrPath ::
  (Text -> IO ()) ->
  Text ->
  UpdateEnv ->
  Text ->
  IO UpdatePackageResult
updateAttrPath log mergeBase updateEnv@UpdateEnv {..} attrPath = do
  log $ "attrpath: " <> attrPath
  let pr = doPR options

  successOrFailure <- runExceptT $ do
    hasUpdateScript <- Nix.hasUpdateScript attrPath

    existingCommitMsg <- fmap getAlt . execWriterT $
      whenBatch updateEnv do
        Skiplist.attrPath attrPath
        when pr do
          liftIO $ log "Checking auto update branch..."
          mbLastCommitMsg <- lift $ Git.findAutoUpdateBranchMessage packageName
          tell $ Alt mbLastCommitMsg
          unless hasUpdateScript do
            lift $ checkExistingUpdate log updateEnv mbLastCommitMsg attrPath

    unless hasUpdateScript do
      Nix.assertNewerVersion updateEnv
      Version.assertCompatibleWithPathPin updateEnv attrPath

    let skipOutpathBase = either Just (const Nothing) $ Skiplist.skipOutpathCalc packageName

    derivationFile <- Nix.getDerivationFile attrPath
    unless hasUpdateScript do
      assertNotUpdatedOn updateEnv derivationFile "master"
      assertNotUpdatedOn updateEnv derivationFile "staging"
      assertNotUpdatedOn updateEnv derivationFile "staging-next"

    -- Calculate output paths for rebuilds and our merge base
    let calcOutpaths = calculateOutpaths options && isNothing skipOutpathBase
    mergeBaseOutpathSet <-
      if calcOutpaths
        then Outpaths.currentOutpathSet
        else return $ Outpaths.dummyOutpathSetBefore attrPath

    -- Get the original values for diffing purposes
    derivationContents <- liftIO $ T.readFile $ T.unpack derivationFile
    oldHash <- Nix.getHash attrPath <|> pure ""
    oldSrcUrl <- Nix.getSrcUrl attrPath <|> pure ""
    oldRev <- Nix.getAttrString "rev" attrPath <|> pure ""
    oldVerMay <- rightMay `fmapRT` (lift $ runExceptT $ Nix.getAttrString "version" attrPath)

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
    rewriteMsgs <- Rewrite.runAll log Rewrite.Args {derivationFile = T.unpack derivationFile, ..}
    ----------------------------------------------------------------------------

    -- Compute the diff and get updated values
    diffAfterRewrites <- Git.diff mergeBase
    tryAssert
      "The diff was empty after rewrites."
      (diffAfterRewrites /= T.empty)
    lift . log $ "Diff after rewrites:\n" <> diffAfterRewrites
    updatedDerivationContents <- liftIO $ T.readFile $ T.unpack derivationFile
    newSrcUrl <- Nix.getSrcUrl attrPath <|> pure ""
    newHash <- Nix.getHash attrPath <|> pure ""
    newRev <- Nix.getAttrString "rev" attrPath <|> pure ""
    newVerMay <- rightMay `fmapRT` (lift $ runExceptT $ Nix.getAttrString "version" attrPath)

    tryAssert
      "The derivation has no 'version' attribute, so do not know how to figure out the version while doing an updateScript update"
      (not hasUpdateScript || isJust newVerMay)

    -- Sanity checks to make sure the PR is worth opening
    unless hasUpdateScript do
      when (derivationContents == updatedDerivationContents) $ throwE "No rewrites performed on derivation."
      when (oldSrcUrl /= "" && oldSrcUrl == newSrcUrl) $ throwE "Source url did not change. "
      when (oldHash /= "" && oldHash == newHash) $ throwE "Hashes equal; no update necessary"
      when (oldRev /= "" && oldRev == newRev) $ throwE "rev equal; no update necessary"

    --
    -- Update updateEnv if using updateScript
    updateEnv' <-
      if hasUpdateScript
        then do
          -- Already checked that these are Just above.
          let oldVer = fromJust oldVerMay
          let newVer = fromJust newVerMay

          -- Some update scripts make file changes but don't update the package
          -- version; ignore these updates (#388)
          when (newVer == oldVer) $ throwE "Package version did not change."

          return $
            UpdateEnv
              packageName
              oldVer
              newVer
              (Just "passthru.updateScript")
              options
        else return updateEnv

    whenBatch updateEnv do
      when pr do
        when hasUpdateScript do
          checkExistingUpdate log updateEnv' existingCommitMsg attrPath

    when hasUpdateScript do
      changedFiles <- Git.diffFileNames mergeBase
      let rewrittenFile = case changedFiles of [f] -> f; _ -> derivationFile
      assertNotUpdatedOn updateEnv' rewrittenFile "master"
      assertNotUpdatedOn updateEnv' rewrittenFile "staging"
      assertNotUpdatedOn updateEnv' rewrittenFile "staging-next"

    --
    -- Outpaths
    -- this sections is very slow
    editedOutpathSet <- if calcOutpaths then Outpaths.currentOutpathSetUncached else return $ Outpaths.dummyOutpathSetAfter attrPath
    let opDiff = S.difference mergeBaseOutpathSet editedOutpathSet
    let numPRebuilds = Outpaths.numPackageRebuilds opDiff
    whenBatch updateEnv do
      Skiplist.python numPRebuilds derivationContents
    when (numPRebuilds == 0) (throwE "Update edits cause no rebuilds.")
    -- end outpaths section

    Nix.build attrPath

    --
    -- Publish the result
    lift . log $ "Successfully finished processing"
    result <- Nix.resultLink
    let opReport =
          if isJust skipOutpathBase
            then "Outpath calculations were skipped for this package; total number of rebuilds unknown."
            else Outpaths.outpathReport opDiff
    let prBase =
          flip
            fromMaybe
            skipOutpathBase
            if Outpaths.numPackageRebuilds opDiff <= 500
              then "master"
              else "staging"
    publishPackage log updateEnv' oldSrcUrl newSrcUrl attrPath result opReport prBase rewriteMsgs (isJust existingCommitMsg)

  case successOrFailure of
    Left failure -> do
      log failure
      return UpdatePackageFailure
    Right () -> return UpdatePackageSuccess

publishPackage ::
  (Text -> IO ()) ->
  UpdateEnv ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  [Text] ->
  Bool ->
  ExceptT Text IO ()
publishPackage log updateEnv oldSrcUrl newSrcUrl attrPath result opReport prBase rewriteMsgs branchExists = do
  cachixTestInstructions <- doCachix log updateEnv result
  resultCheckReport <-
    case Skiplist.checkResult (packageName updateEnv) of
      Right () -> lift $ Check.result updateEnv (T.unpack result)
      Left msg -> pure msg
  metaDescription <- Nix.getDescription attrPath <|> return T.empty
  metaHomepage <- Nix.getHomepage attrPath <|> return T.empty
  metaChangelog <- Nix.getChangelog attrPath <|> return T.empty
  cveRep <- liftIO $ cveReport updateEnv
  releaseUrl <- GH.releaseUrl updateEnv newSrcUrl <|> return ""
  compareUrl <- GH.compareUrl oldSrcUrl newSrcUrl <|> return ""
  maintainers <- Nix.getMaintainers attrPath
  let commitMsg = commitMessage updateEnv attrPath
  -- Wait for OfBorg before committing, so that delete-done can use the date of
  -- the commit to avoid deleting new commits
  ofBorgWaitUntil <- lift $ addUTCTime (fromInteger $ 60 * 60) <$> getCurrentTime
  when
    (batchUpdate . options $ updateEnv)
    (lift (untilOfBorgFree log ofBorgWaitUntil))
  Git.commit commitMsg
  commitRev <- Git.headRev
  nixpkgsReviewMsg <-
    if prBase /= "staging" && (runNixpkgsReview . options $ updateEnv)
      then liftIO $ NixpkgsReview.runReport log commitRev
      else return ""
  isBroken <- Nix.getIsBroken attrPath
  -- Try to push it three times
  -- (these pushes use --force, so it doesn't matter if branchExists is True)
  when
    (doPR . options $ updateEnv)
    (Git.push updateEnv <|> Git.push updateEnv <|> Git.push updateEnv)
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
          commitRev
          attrPath
          maintainers
          result
          opReport
          cveRep
          cachixTestInstructions
          nixpkgsReviewMsg
  liftIO $ log prMsg
  if (doPR . options $ updateEnv)
    then do
      let ghUser = GH.untagName . githubUser . options $ updateEnv
      let mkPR = if branchExists then GH.prUpdate else GH.pr
      (reusedPR, pullRequestUrl) <- mkPR updateEnv (prTitle updateEnv attrPath) prMsg (ghUser <> ":" <> (branchName updateEnv)) prBase
      when branchExists $
        liftIO $
          log
            if reusedPR
              then "Updated existing PR"
              else "Reused existing auto update branch, but no corresponding open PR was found, so created a new PR"
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
prMessage updateEnv isBroken metaDescription metaHomepage metaChangelog rewriteMsgs releaseUrl compareUrl resultCheckReport commitRev attrPath maintainers resultPath opReport cveRep cachixTestInstructions nixpkgsReviewMsg =
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
        if metaChangelog == T.empty
          then ""
          else "meta.changelog for " <> attrPath <> " is: " <> metaChangelog
      rewriteMsgsLine = foldl (\ms m -> ms <> T.pack "\n- " <> m) "\n###### Updates performed" rewriteMsgs
      maintainersCc =
        if not (T.null maintainers)
          then "cc " <> maintainers <> " for [testing](https://github.com/ryantm/nixpkgs-update/blob/main/doc/nixpkgs-maintainer-faq.md#r-ryantm-opened-a-pr-for-my-package-what-do-i-do)."
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

       <b>Checks done</b>

       ---

       - built on NixOS
       $resultCheckReport

       ---

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
       nix-build -A $attrPath https://github.com/$ghUser/nixpkgs/archive/$commitRev.tar.gz
       ```
       Or:
       ```
       nix build github:$ghUser/nixpkgs/$commitRev#$attrPath
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

untilOfBorgFree :: (Text -> IO ()) -> UTCTime -> IO ()
untilOfBorgFree log waitUntil = do
  now <- getCurrentTime
  when (now < waitUntil) do
    stats <-
      shell "curl -s https://events.ofborg.org/stats.php" & readProcessInterleaved_
    waiting <-
      shell (jqBin <> " .evaluator.messages.waiting")
        & setStdin (byteStringInput stats)
        & readProcessInterleaved_
        & fmap (BSL.readInt >>> fmap fst >>> fromMaybe 0)
    when (waiting > 2) $ do
      liftIO $ log ("Waiting for OfBorg: https://events.ofborg.org/stats.php's evaluator.messages.waiting = " <> tshow waiting)
      liftIO $ threadDelay 60000000
      untilOfBorgFree log waitUntil

assertNotUpdatedOn ::
  (MonadIO m) => UpdateEnv -> Text -> Text -> ExceptT Text m ()
assertNotUpdatedOn updateEnv derivationFile branch = do
  derivationContents <- Git.show branch derivationFile
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

doCachix :: (MonadIO m) => (Text -> m ()) -> UpdateEnv -> Text -> ExceptT Text m Text
doCachix log updateEnv resultPath =
  let o = options updateEnv
   in if batchUpdate o && "r-ryantm" == (GH.untagName $ githubUser o)
        then do
          lift $ log ("cachix " <> (T.pack . show) resultPath)
          Nix.cachix resultPath
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
  IO ()
updatePackage o updateInfo = do
  let (p, oldV, newV, url) = head (rights (parseUpdates updateInfo))
  let updateInfoLine = (p <> " " <> oldV <> " -> " <> newV <> fromMaybe "" (fmap (" " <>) url))
  let updateEnv = UpdateEnv p oldV newV url o
  let log = T.putStrLn
  liftIO $ notifyOptions log o
  updated <- updatePackageBatch log updateInfoLine updateEnv
  case updated of
    UpdatePackageFailure -> do
      log $ "[result] Failed to update " <> updateInfoLine
    UpdatePackageSuccess -> do
      log $ "[result] Success updating " <> updateInfoLine

withWorktree :: Text -> Text -> UpdateEnv -> IO a -> IO a
withWorktree branch attrpath updateEnv action = do
  bracket
    ( do
        dir <- U.worktreeDir
        let path = dir <> "/" <> T.unpack (T.replace ".lock" "_lock" attrpath)
        Git.worktreeRemove path
        Git.delete1 (branchName updateEnv)
        Git.worktreeAdd path branch updateEnv
        pure path
    )
    ( \path -> do
        Git.worktreeRemove path
        Git.delete1 (branchName updateEnv)
    )
    (\path -> withCurrentDirectory path action)
