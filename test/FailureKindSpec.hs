{-# LANGUAGE OverloadedStrings #-}

module FailureKindSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Set as S
import qualified Data.Text as T
import FailureKind
import Test.Hspec

spec :: Spec
spec = do
  describe "failureKindCode / failureKindFromCode" do
    it "round-trips every constructor" do
      let kinds = [minBound .. maxBound] :: [FailureKind]
      mapM_
        ( \k ->
            failureKindFromCode (failureKindCode k) `shouldBe` Just k
        )
        kinds

    it "rejects unknown codes" do
      failureKindFromCode "not_a_real_kind" `shouldBe` Nothing

  describe "ToJSON / FromJSON" do
    it "round-trips FailureKind" do
      let kinds = [minBound .. maxBound] :: [FailureKind]
      mapM_
        ( \k ->
            Aeson.decode (Aeson.encode k) `shouldBe` Just k
        )
        kinds

  describe "classifyFailureMessage (golden samples)" do
    let golden :: [(T.Text, FailureKind)]
        golden =
          [ ( "nix build failed.\nlast lines of log\n ",
              NixBuildFailed
            ),
            ( "Received ExitFailure 1 when running\nRaw command: /nix/store/.../bin/nix-build -A dotenvx\nnix build failed.\nnpm error\n ",
              NixBuildFailed
            ),
            ( "nix log failed trying to get build logs ",
              NixLogFetchFailed
            ),
            ( "Failed to read expected nix boolean weird ",
              NixBooleanParseFailed
            ),
            ( "Could not find result link. ",
              ResultLinkMissing
            ),
            ( "build succeeded unexpectedly",
              FixedOutputProbeUnexpectedSuccess
            ),
            ( "2.0 is not newer than 2.0 according to Nix; versionComparison: 0 ",
              VersionNotNewerPerNix
            ),
            ( "An auto update branch exists with an equal or greater version",
              AutoUpdateBranchVersionBlocks
            ),
            ( "No rewrites performed on derivation.",
              NoRewritesPerformed
            ),
            ( "Source url did not change. ",
              SourceUrlUnchanged
            ),
            ( "Hashes equal; no update necessary",
              FixedOutputHashUnchanged
            ),
            ( "nix build failed.\nfixed-output derivation produced path '/nix/store/abc-source' with sha256 hash 'got' instead of the expected hash 'want'\n ",
              FetchHashMismatch
            ),
            ( "rev equal; no update necessary",
              RevUnchanged
            ),
            ( "Package version did not change.",
              PackageVersionUnchanged
            ),
            ( "Update edits cause no rebuilds.",
              NoRebuildsFromEdits
            ),
            ( "cargo hashes equal; no update necessary: abc",
              CargoHashUnchanged
            ),
            ( "deps hashes equal; no update necessary: abc",
              DepsHashUnchanged
            ),
            ( "[updateScript] Failed with exit code 1\nsomething",
              UpdateScriptFailed
            ),
            ( "[updateScript] Failed with exit code 124\nupdateScript for foo took longer than 30m and timed out. Other output: ",
              UpdateScriptTimedOut
            ),
            ( "Too many open PRs from user:auto-update/foo",
              GitHubTooManyOpenPRs
            ),
            ( "There might already be an open PR for this update:\n- title\n  url",
              GitHubPossibleDuplicatePR
            ),
            ( "GitHub: https://example.com/foo is not a GitHub URL.",
              GitHubUrlNotGitHub
            ),
            ( "Unable to parse update: a b",
              UpdateInfoParseFailed
            ),
            ( "Failed to create log directory.",
              LogDirectoryCreationFailed
            ),
            ( "LOGS_DIRECTORY /tmp/x does not exist.",
              LogDirectoryResolutionFailed
            ),
            ( "XDG_RUNTIME_DIR /run/user/1 does not exist.",
              XdgRuntimeDirResolutionFailed
            ),
            ( "Temporary directory /tmp/nixpkgs-updateXXXX does not exist.",
              TemporaryDirectoryResolutionFailed
            ),
            ( "The diff was empty after rewrites.",
              DiffEmptyAfterRewrites
            ),
            ( "The derivation has no 'version' attribute, so do not know how to figure out the version while doing an updateScript update",
              DerivationMissingVersionAttribute
            ),
            ( "Old version 1.0\" not present in master derivation file with contents: {}",
              OldVersionMissingOnBranch
            ),
            ( "Version in attr path foo not compatible with 9.0",
              AttrPathVersionPinMismatch
            ),
            ( "Python package with too many package rebuilds 200  > 100",
              PythonRebuildLimitExceeded
            ),
            ( "grep did not find version in file names",
              VersionCheckGrepFailed
            ),
            ( "stderr did not split as expected full stderr was: \n",
              FixedOutputStderrParseFailed
            ),
            ( "Packages for lxqt are currently skipped.",
              PolicySkiplist
            ),
            ( "SomeHttpExceptionRequest StatusCodeException",
              GitHubApiError
            ),
            ( "completely unknown free text from a subprocess",
              Unclassified
            )
          ]

    it "matches expected kind for each sample" do
      mapM_
        ( \(msg, expected) ->
            classifyFailureMessage msg `shouldBe` expected
        )
        golden

  describe "failureKindCode uniqueness" do
    it "uses distinct codes for each constructor" do
      let kinds = [minBound .. maxBound] :: [FailureKind]
          codes = map failureKindCode kinds
      S.size (S.fromList codes) `shouldBe` length kinds

  describe "JSON encoding shape" do
    it "encodes as a JSON string (stable for Rust consumers)" do
      BLC.unpack (Aeson.encode NixBuildFailed) `shouldBe` "\"nix_build_failed\""
