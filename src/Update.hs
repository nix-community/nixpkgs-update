{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Update
    ( updateAll
    ) where

import Check (checkResult)
import Clean (fixSrcUrl)
import Control.Exception (throw)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Shelly
import Utils (ExitCode(..), Options(..), Version, canFail, checkAttrPathVersion, orElse, parseUpdates, setupNixpkgs, tRead)

default (T.Text)

cleanup :: Text -> Sh ()
cleanup branchName = do
    cmd "git" "reset" "--hard"
    cmd "git" "clean" "-fd"
    cmd "git" "checkout" "-B" "master" "upstream/master"
    cmd "git" "reset" "--hard" "upstream/master"
    canFail $ cmd "git" "branch" "-D" branchName

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
    , (("libxc" `T.isInfixOf`), "currently people don't want to update this https://github.com/NixOS/nixpkgs/pull/35821")
    , (("perl" `T.isInfixOf`), "currently don't know how to update perl")
    , (("python" `T.isInfixOf`), "currently don't know how to update python")
    , (("cdrtools" `T.isInfixOf`), "We keep downgrading this by accident.")
    , (("gst" `T.isInfixOf`), "gstreamer plugins are kept in lockstep.")
    , (("electron" `T.isInfixOf`), "multi-platform srcs in file.")
    , (("linux-headers" `T.isInfixOf`), "Not updated until many packages depend on it (part of stdenv).")
    , (("mpich" `T.isInfixOf`), "Reported on repology.org as mischaracterized newest version")
    , (("xfce" `T.isInfixOf`), "@volth asked to not update xfce")
    , (("cmake-cursesUI-qt4UI" `T.isInfixOf`), "Derivation file is complicated")
    , (("varnish" `T.isInfixOf`), "Temporary blacklist because of multiple versions and slow nixpkgs update")
    , (("iana-etc" `T.isInfixOf`), "@mic92 takes care of this package")
    , ((== "isl"), "multi-version long building package")
    , ((== "tokei"), "got stuck forever building with no CPU usage")
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

nixEval' :: (Text -> Sh Text) -> Text -> Sh Text
nixEval' errorExit expr =
    (T.strip <$> cmd "nix" "eval" "-f" "." expr) `orElse`
        errorExit ("nix eval failed for " <> expr)

rawEval' :: (Text -> Sh Text) -> Text -> Sh Text
rawEval' errorExit expr =
    (T.strip <$> cmd "nix" "eval" "-f" "." "--raw" expr) `orElse`
        errorExit ("raw nix eval failed for " <> expr)

push :: Text -> Options -> Sh ()
push branchName options =
    if dryRun options then
        return ()
    else
        cmd "git" "push" "--set-upstream" "origin" branchName "--force"

log' logFile msg = do
    -- TODO: switch to Data.Time.Format.ISO8601 once time-1.9.0 is available
    runDate <- T.pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) <$> liftIO getCurrentTime
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

    currentTime <- liftIO getCurrentTime
    updateLoop options log (parseUpdates updates) currentTime

updateLoop :: Options -> (Text -> Sh ()) -> [(Text, Version, Version)] -> UTCTime -> Sh ()
updateLoop _ log [] _ = log "ups.sh finished"
updateLoop options log ((package, oldVersion, newVersion):moreUpdates) okToPrAt = do
    log package

    updated <- catch_sh
      (updatePackage options log package oldVersion newVersion okToPrAt)
      (\ e ->
         case e of
           ExitCode 0 -> return True
           ExitCode _ -> return False)

    okToPrAt <-
        if updated then do
            log "SUCCESS"
            -- current time + 15 minutes
            addUTCTime (fromInteger $ 15 * 60) <$> liftIO getCurrentTime
        else do
            log "FAIL"
            return okToPrAt

    updateLoop options log moreUpdates okToPrAt

updatePackage :: Options -> (Text -> Sh ()) -> Text -> Version -> Version -> UTCTime -> Sh Bool
updatePackage options log packageName oldVersion newVersion okToPrAt = do
    nixpkgsPath <- setupNixpkgs

    setenv "NIX_PATH" ("nixpkgs=" <> toTextIgnore nixpkgsPath)

    let branchName = "auto-update/" <> packageName

    let errorExit = errorExit' log branchName

    let nixEval = nixEval' errorExit

    let rawEval = rawEval' errorExit

    versionComparison <- T.strip <$> cmd "nix" "eval" "-f" "." ("(builtins.compareVersions \"" <> newVersion <> "\" \"" <> oldVersion <> "\")")

    unless (versionComparison == "1") $ do
        errorExit $ newVersion <> " is not newer than " <> oldVersion <> " according to Nix; versionComparison: " <> versionComparison

    -- Package blacklist
    forM_ nameBlackList $ \(isBlacklisted, message) -> do
        when (isBlacklisted packageName) $ errorExit message

    oneHourAgo <- cmd "date" "+%s" "-d" "-1 hour"
    fetchedLast <- cmd "stat" "-c" "%Y" ".git/FETCH_HEAD"
    when (fetchedLast < oneHourAgo) $ do
        canFail $ cmd "git" "fetch" "--prune" "--multiple" "upstream" "origin"

    remotes <- T.lines <$> cmd "git" "branch" "--remote"
    when (("origin/auto-update/" <> packageName) `elem` remotes) $ do
        errorExit "Update branch already on origin."

    cmd "git" "reset" "--hard"
    cmd "git" "clean" "-fd"
    cmd "git" "checkout" "-B" "master" "upstream/master"
    cmd "git" "reset" "--hard" "upstream/master"
    cmd "git" "clean" "-fd"

    -- This is extremely slow but will give us better results
    attrPath <- head . T.words . head . T.lines <$> cmd "nix-env" "-qa" (packageName <> "-" <> oldVersion) "-f" "." "--attr-path" `orElse`
        errorExit "nix-env -q failed to find package name with old version"

    -- Temporarily blacklist gnome sources for lockstep update
    whenM (("gnome" `T.isInfixOf`) <$> (nixEval ("pkgs." <> attrPath <> ".src.urls"))) $ do
        errorExit "Packages from gnome are currently blacklisted."

    -- Temporarily blacklist lua packages at @teto's request
    -- https://github.com/NixOS/nixpkgs/pull/37501#issuecomment-375169646
    when (T.isPrefixOf "lua" attrPath) $ do
        errorExit "Packages for lua are currently blacklisted."

    derivationFile <- fromText . T.strip <$> cmd "env" "EDITOR=echo" "nix" "edit" attrPath "-f" "." `orElse` errorExit "Couldn't find derivation file."

    flip catchany_sh (\ e -> do
                         cleanup branchName
                         errorExit (T.pack (show e))
                     ) $ do
        numberOfFetchers <- tRead <$> canFail (cmd "grep" "-c" "fetchurl {|fetchgit {|fetchFromGitHub {" derivationFile)
        unless ((numberOfFetchers :: Int) <= 1) $ do
            errorExit $ "More than one fetcher in " <> toTextIgnore derivationFile

        derivationContents <- readfile derivationFile

        forM_ contentBlacklist $ \(offendingContent, message) -> do
            when (offendingContent `T.isInfixOf` derivationContents) $ errorExit message

        unless (checkAttrPathVersion attrPath newVersion) $ do
            errorExit ("Version in attr path " <> attrPath <> " not compatible with " <> newVersion)

        -- Make sure it hasn't been updated on master
        cmd "grep" oldVersion derivationFile `orElse`
            errorExit "Old version not present in master derivation file."

        -- Make sure it hasn't been updated on staging
        cmd "git" "reset" "--hard"
        cmd "git" "clean" "-fd"
        cmd "git" "checkout" "-B" "staging" "upstream/staging"
        cmd "git" "reset" "--hard" "upstream/staging"
        cmd "git" "clean" "-fd"

        cmd "grep" oldVersion derivationFile `orElse`
            errorExit "Old version not present in staging derivation file."

        base <- T.strip <$> cmd "git" "merge-base" "upstream/master" "upstream/staging"

        cmd "git" "checkout" "-B" branchName base

        oldHash <- rawEval ("pkgs." <> attrPath <> ".src.drvAttrs.outputHash") `orElse`
            errorExit ("Could not find old output hash at " <> attrPath <> ".src.drvAttrs.outputHash.")

        oldSrcUrl <- rawEval ("(let pkgs = import ./. {}; in builtins.elemAt pkgs." <> attrPath <> ".src.drvAttrs.urls 0)")

        cmd "sed" "-i" ("s/" <> (T.replace "." "\\." oldVersion) <> "/" <> newVersion <> "/g") derivationFile `orElse`
            errorExit "Could not replace oldVersion with newVersion."

        newSrcUrl <- rawEval ("(let pkgs = import ./. {}; in builtins.elemAt pkgs." <> attrPath <> ".src.drvAttrs.urls 0)")

        when (oldSrcUrl == newSrcUrl) $ do
            errorExit "Source url did not change."

        newHash <- T.strip <$> cmd "nix-prefetch-url" "-A" (attrPath <> ".src") `orElse`
            fixSrcUrl packageName oldVersion newVersion derivationFile attrPath oldSrcUrl `orElse`
            errorExit "Could not prefetch new version URL."

        when (oldHash == newHash) $ do
            errorExit "Hashes equal; no update necessary"

        cmd "sed" "-i" ("s/" <> oldHash <> "/" <> newHash <> "/g") derivationFile `orElse`
            errorExit "Could not replace oldHash with newHash."

        cmd "rm" "-f" "result*"

        cmd "nix" "build" "-f" "." attrPath `orElse` do
            buildLog <- T.unlines . reverse . take 30 . reverse . T.lines <$> cmd "nix" "log" "-f" "." attrPath
            errorExit ("nix build failed.\n" <> buildLog)

        result <- fromText <$> (cmd "readlink" "./result" `orElse` cmd "readlink" "./result-bin") `orElse`
            errorExit "Could not find result link."

        resultCheckReport <- checkResult options result newVersion

        hasMaintainers <- const True <$> nixEval ("(let pkgs = import ./. {}; in pkgs." <> attrPath <> ".meta.maintainers)") `orElse` return False
        maintainers <- if hasMaintainers then rawEval ("(let pkgs = import ./. {}; gh = m : m.github or \"\"; nonempty = s: s != \"\"; addAt = s: \"@\"+s; in builtins.concatStringsSep \" \" (map addAt (builtins.filter nonempty (map gh pkgs." <> attrPath <> ".meta.maintainers))))") else return ""

        let maintainersCc = if not (T.null maintainers) then "\n\ncc " <> maintainers <> " for review" else ""

        cmd "git" "diff"

        let
            commitMessage :: Text
            commitMessage = attrPath <> ": " <> oldVersion <> " → " <> newVersion <> "\n\n"
               <> "Semi-automatic update generated by https://github.com/ryantm/nixpkgs-update tools.\n\n"
               <> "This update was made based on information from https://repology.org/metapackage/" <> packageName <> "/versions.\n\n"
               <> "These checks were done:\n\n"
               <> "- built on NixOS\n"
               <> resultCheckReport

        cmd "git" "commit" "-am" commitMessage

        -- Try to push it three times
        push branchName options `orElse` push branchName options `orElse` push branchName options

        isBroken <- nixEval ("(let pkgs = import ./. {}; in pkgs." <> attrPath <> ".meta.broken or false)")
        let brokenWarning = if isBroken == "true" then "" else "- WARNING: Package has meta.broken=true; Please manually test this package update and remove the broken attribute."

        let prMessage = commitMessage <> brokenWarning <> maintainersCc

        unless (dryRun options) $ do
            current <- liftIO getCurrentTime
            when (current < okToPrAt) $ do
                let sleepSeconds = diffUTCTime okToPrAt current
                echo $ "Sleeping " <> T.pack (show sleepSeconds) <> " seconds."
                -- TODO: use nominalDiffTimeToSeconds once time-1.9.1 is available
                sleep (fromEnum sleepSeconds)

            cmd "hub" "pull-request" "-m" prMessage

        cmd "git" "reset" "--hard"
        cmd "git" "clean" "-fd"
        cmd "git" "checkout" "-B" "master" "upstream/master"
        cmd "git" "reset" "--hard"
        cmd "git" "clean" "-fd"

        return True
