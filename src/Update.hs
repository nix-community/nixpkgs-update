{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Update (updatePackage) where

import Control.Exception (throw)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Shelly
import Check (checkResult)
import Utils (Version, Options(..), ExitCode(..), canFail, orElse, setupNixpkgs, tRead, checkAttrPathVersion)
import Data.Semigroup ((<>))
default (T.Text)

cleanup :: Text -> Sh ()
cleanup branchName = do
    cmd "git" "reset" "--hard"
    cmd "git" "clean" "-fd"
    cmd "git" "checkout" "-B" "master" "upstream/master"
    cmd "git" "reset" "--hard" "upstream/master"
    canFail $ cmd "git" "branch" "-D" branchName

errorExit' :: Text -> Text -> Sh a
errorExit' branchName message = do
    cleanup branchName
    echo $ "$(date -Iseconds) " <> message
    throw (ExitCode 1)

isOnBlackList :: (Text -> Sh Text) -> Text -> Sh Text
isOnBlackList errorExit packageName
    | "jquery" `T.isInfixOf` packageName = errorExit "this isn't a real package"
    | "google-cloud-sdk" `T.isInfixOf` packageName = errorExit "complicated package"
    | "github-release" `T.isInfixOf` packageName = errorExit "complicated package"
    | "fcitx" `T.isInfixOf` packageName = errorExit "gets stuck in daemons"
    | "libxc" `T.isInfixOf` packageName = errorExit "currently people don't want to update this https://github.com/NixOS/nixpkgs/pull/35821"
    | "perl" `T.isInfixOf` packageName = errorExit "currently don't know how to update perl"
    | "python" `T.isInfixOf` packageName = errorExit "currently don't know how to update python"
    | "cdrtools" `T.isInfixOf` packageName = errorExit "We keep downgrading this by accident."
    | "gst" `T.isInfixOf` packageName = errorExit "gstreamer plugins are kept in lockstep."
    | "electron" `T.isInfixOf` packageName = errorExit "multi-platform srcs in file."
    | "linux-headers" `T.isInfixOf` packageName = errorExit "Not updated until many packages depend on it (part of stdenv)."
    | "mpich" `T.isInfixOf` packageName = errorExit "Reported on repology.org as mischaracterized newest version"
    | "xfce" `T.isInfixOf` packageName = errorExit "@volth asked to not update xfce"
    | "cmake-cursesUI-qt4UI" `T.isInfixOf` packageName = errorExit "Derivation file is complicated"
    | "varnish" `T.isInfixOf` packageName = errorExit "Temporary blacklist because of multiple versions and slow nixpkgs update"
    | "iana-etc" `T.isInfixOf` packageName = errorExit "@mic92 takes care of this package"
isOnBlackList errorExit "isl" = errorExit "multi-version long building package"
isOnBlackList errorExit "tokei" = errorExit "got stuck forever building with no CPU usage"
isOnBlackList _ _ = return ""

rawEval :: Text -> Sh Text
rawEval expr = cmd "nix" "eval" "-f" "." "--raw" expr

fixSrcUrl :: Text -> Version -> Version -> Text -> Text -> Text -> Sh Text
fixSrcUrl packageName oldVersion newVersion derivationFile attrPath oldSrcUrl = cmd "./fix-src-url.sh" packageName oldVersion newVersion derivationFile attrPath oldSrcUrl

push :: Text -> Options -> Sh ()
push branchName options =
    if dryRun options then
        return ()
    else
        cmd "git" "push" "--set-upstream" "origin" branchName "--force"

updatePackage :: Options -> Text -> Version -> Version -> Int -> Sh Bool
updatePackage options packageName oldVersion newVersion okToPrAt = do
    nixpkgsPath <- setupNixpkgs

    setenv "NIX_PATH" ("nixpkgs=" <> toTextIgnore nixpkgsPath)

    let branchName = "auto-update/" <> packageName

    let errorExit = errorExit' branchName

    versionComparison <- cmd "nix" "eval" "-f" "." ("(builtins.compareVersions \"" <> newVersion <> "\" \"" <> oldVersion <> "\")")

    unless (versionComparison == "1\n") $ do
        errorExit $ newVersion <> " is not newer than " <> oldVersion <> " according to Nix; versionComparison: " <> versionComparison

    -- Package blacklist
    isOnBlackList errorExit packageName

    oneHourAgo <- cmd "date" "+%s" "-d" "-1 hour"
    fetchedLast <- cmd "stat" "-c" "%Y" ".git/FETCH_HEAD"
    when (fetchedLast < oneHourAgo) $ do
        canFail $ cmd "git" "fetch" "--prune" "--multiple" "upstream" "origin"

    remotes <- T.lines <$> cmd "git" "branch" "--remote"
    when (("origin/auto-update/" <> packageName) `elem` remotes) $ do
        errorExit "Update branch already on origin."

    cmd "git" "reset" "--hard"
    cmd "git" "clean" "-fd"
    cmd "git" "checkout" "master"
    cmd "git" "reset" "--hard" "upstream/master"
    cmd "git" "clean" "-fd"

    -- This is extremely slow but will give us better results
    attrPath <- head . T.words . head . T.lines <$> cmd "nix-env" "-qa" (packageName <> "-" <> oldVersion) "-f" "." "--attr-path" `orElse`
        errorExit "nix-env -q failed to find package name with old version"

    -- Temporarily blacklist gnome sources for lockstep update
    whenM (("gnome" `T.isInfixOf`) <$> cmd "nix" "eval" "-f" "." ("pkgs." <> attrPath <> ".src.urls")) $ do
        errorExit "Packages from gnome are currently blacklisted."

    -- Temporarily blacklist lua packages at @teto's request
    -- https://github.com/NixOS/nixpkgs/pull/37501#issuecomment-375169646
    when (T.isPrefixOf "lua" attrPath) $ do
        errorExit "Packages for lua are currently blacklisted."


    derivationFile <- cmd "env" "EDITOR=echo" "nix" "edit" attrPath "-f" "." `orElse` errorExit "Couldn't find derivation file."


    flip finally_sh (cleanup branchName) $ do
        numberOfFetchers <- tRead <$> cmd "grep" "-c" "fetchurl {|fetchgit {|fetchFromGitHub {" derivationFile
        unless ((numberOfFetchers :: Int) <= 1) $ do
            errorExit $ "More than one fetcher in " <> derivationFile

        derivationContents <- cmd "cat" derivationFile

        if T.isInfixOf "DO NOT EDIT" derivationContents then
            errorExit "Derivation file says not to edit it."
        -- Skip packages that have special builders
        else if T.isInfixOf "buildGoPackage" derivationContents then
            errorExit "Derivation contains buildGoPackage."
        else if T.isInfixOf "buildRustCrate" derivationContents then
            errorExit "Derivation contains buildRustCrate."
        else if T.isInfixOf "buildPythonPackage" derivationContents then
            errorExit "Derivation contains buildPythonPackage."
        else if T.isInfixOf "buildRubyGem" derivationContents then
            errorExit "Derivation contains buildRubyGem."
        else if T.isInfixOf "bundlerEnv" derivationContents then
            errorExit "Derivation contains bundlerEnv."
        else if T.isInfixOf "buildPerlPackage" derivationContents then
            errorExit "Derivation contains buildPerlPackage."
        else return ()

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

        base <- cmd "git" "merge-base" "upstream/master" "upstream/staging"
        cmd "git" "checkout" "-B" branchName base

        oldHash <- rawEval ("pkgs." <> attrPath <> ".src.drvAttrs.outputHash") `orElse`
            errorExit ("Could not find old output hash at " <> attrPath <> ".src.drvAttrs.outputHash.")

        oldSrcUrl <- rawEval ("let pkgs = import ./. {}; in builtins.elemAt pkgs." <> attrPath <> ".src.drvAttrs.urls 0)")

        cmd "sed" "-i" ("s/" <> (T.replace "." "\\." oldVersion) <> "/" <> newVersion <> "/g") derivationFile `orElse`
            errorExit "Could not replace oldVersion with newVersion."

        newSrcUrl <- rawEval ("let pkgs = import ./. {}; in builtins.elemAt pkgs." <> attrPath <> ".src.drvAttrs.urls 0")

        when (oldSrcUrl == newSrcUrl) $ do
            errorExit "Source url did not change."

        newHash <- cmd "nix-prefetch-url" "-A" (attrPath <> ".src") `orElse`
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

        resultCheckReport <- checkResult result newVersion

        hasMaintainers <- const True <$> cmd "nix" "eval" ("let pkgs = import ./. {}; in pkgs." <> attrPath <> ".meta.maintainers") `orElse` return False
        maintainers <- if hasMaintainers then rawEval ("let pkgs = import ./. {}; gh = m : m.github or \"\"; nonempty = s: s != \"\"; addat = s: \"@\"+s; in builtins.concatStringsSep \" \" (map addat (builtins.filter nonempty (map gh pkgs." <> attrPath <> ".meta.maintainers)))") else return ""

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

        isBroken <- cmd "nix" "eval" "-f" "." ("let pkgs = import ./. {}; in pkgs." <> attrPath <> ".meta.broken or false")
        let brokenWarning = if isBroken == "true" then "" else "- WARNING: Package has meta.broken=true; Please manually test this package update and remove the broken attribute."

        let prMessage = commitMessage <> brokenWarning <> maintainersCc

        unless (dryRun options) $ do
            current <- tRead <$> cmd "date" "+%s"
            when (current < okToPrAt) $ do
                let sleepSeconds = okToPrAt - current
                echo $ "Sleeping " <> T.pack (show sleepSeconds) <> " seconds."
                sleep sleepSeconds

            cmd "hub" "pull-request" "-m" prMessage

        cmd "git" "reset" "--hard"
        cmd "git" "clean" "-fd"
        cmd "git" "checkout" "-B" "master" "upstream/master"
        cmd "git" "reset" "--hard"
        cmd "git" "clean" "-fd"

        return True
