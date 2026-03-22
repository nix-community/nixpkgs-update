{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stable classification for update failures.
--
-- Messages originate from 'ExceptT Text' paths across the updater. This module
-- maps the historical free-text errors onto a small sum type for metrics,
-- SQLite storage, and future maintainer notification.
module FailureKind
  ( FailureKind (..),
    failureKindCode,
    failureKindFromCode,
    classifyFailureMessage,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Skiplist

-- | High-level failure category. Codes are stable for Rust/SQLite and JSON.
data FailureKind
  = NixBuildFailed
  | NixLogFetchFailed
  | NixBooleanParseFailed
  | ResultLinkMissing
  | FixedOutputProbeUnexpectedSuccess
  | VersionNotNewerPerNix
  | AutoUpdateBranchVersionBlocks
  | NoRewritesPerformed
  | SourceUrlUnchanged
  | FixedOutputHashUnchanged
  | FetchHashMismatch
  | RevUnchanged
  | PackageVersionUnchanged
  | NoRebuildsFromEdits
  | CargoHashUnchanged
  | DepsHashUnchanged
  | UpdateScriptFailed
  | UpdateScriptTimedOut
  | GitHubTooManyOpenPRs
  | GitHubPossibleDuplicatePR
  | GitHubUrlNotGitHub
  | GitHubApiError
  | UpdateInfoParseFailed
  | LogDirectoryCreationFailed
  | LogDirectoryResolutionFailed
  | XdgRuntimeDirResolutionFailed
  | TemporaryDirectoryResolutionFailed
  | DiffEmptyAfterRewrites
  | DerivationMissingVersionAttribute
  | OldVersionMissingOnBranch
  | AttrPathVersionPinMismatch
  | PythonRebuildLimitExceeded
  | VersionCheckGrepFailed
  | PolicySkiplist
  | FixedOutputStderrParseFailed
  | Unclassified
  deriving (Eq, Ord, Show, Enum, Bounded)

failureKindCode :: FailureKind -> T.Text
failureKindCode = \case
  NixBuildFailed -> "nix_build_failed"
  NixLogFetchFailed -> "nix_log_fetch_failed"
  NixBooleanParseFailed -> "nix_boolean_parse_failed"
  ResultLinkMissing -> "result_link_missing"
  FixedOutputProbeUnexpectedSuccess -> "fixed_output_probe_unexpected_success"
  VersionNotNewerPerNix -> "version_not_newer_per_nix"
  AutoUpdateBranchVersionBlocks -> "auto_update_branch_version_blocks"
  NoRewritesPerformed -> "no_rewrites_performed"
  SourceUrlUnchanged -> "source_url_unchanged"
  FixedOutputHashUnchanged -> "fixed_output_hash_unchanged"
  FetchHashMismatch -> "fetch_hash_mismatch"
  RevUnchanged -> "rev_unchanged"
  PackageVersionUnchanged -> "package_version_unchanged"
  NoRebuildsFromEdits -> "no_rebuilds_from_edits"
  CargoHashUnchanged -> "cargo_hash_unchanged"
  DepsHashUnchanged -> "deps_hash_unchanged"
  UpdateScriptFailed -> "update_script_failed"
  UpdateScriptTimedOut -> "update_script_timed_out"
  GitHubTooManyOpenPRs -> "github_too_many_open_prs"
  GitHubPossibleDuplicatePR -> "github_possible_duplicate_pr"
  GitHubUrlNotGitHub -> "github_url_not_github"
  GitHubApiError -> "github_api_error"
  UpdateInfoParseFailed -> "update_info_parse_failed"
  LogDirectoryCreationFailed -> "log_directory_creation_failed"
  LogDirectoryResolutionFailed -> "log_directory_resolution_failed"
  XdgRuntimeDirResolutionFailed -> "xdg_runtime_dir_resolution_failed"
  TemporaryDirectoryResolutionFailed -> "temporary_directory_resolution_failed"
  DiffEmptyAfterRewrites -> "diff_empty_after_rewrites"
  DerivationMissingVersionAttribute -> "derivation_missing_version_attribute"
  OldVersionMissingOnBranch -> "old_version_missing_on_branch"
  AttrPathVersionPinMismatch -> "attr_path_version_pin_mismatch"
  PythonRebuildLimitExceeded -> "python_rebuild_limit_exceeded"
  VersionCheckGrepFailed -> "version_check_grep_failed"
  PolicySkiplist -> "policy_skiplist"
  FixedOutputStderrParseFailed -> "fixed_output_stderr_parse_failed"
  Unclassified -> "unclassified"

codeToKind :: Map.Map T.Text FailureKind
codeToKind =
  Map.fromList $
    map (\k -> (failureKindCode k, k)) [minBound .. maxBound]

failureKindFromCode :: T.Text -> Maybe FailureKind
failureKindFromCode k = Map.lookup k codeToKind

instance ToJSON FailureKind where
  toJSON = toJSON . failureKindCode

instance FromJSON FailureKind where
  parseJSON = withText "FailureKind" $ \t ->
    maybe (fail $ "unknown FailureKind code: " ++ T.unpack t) pure (failureKindFromCode t)

skiplistReasonSet :: S.Set T.Text
skiplistReasonSet = S.fromList Skiplist.thrownSkiplistReasons

-- | Classify a failure message from 'ExceptT Text' (after stripping outer whitespace).
classifyFailureMessage :: Text -> FailureKind
classifyFailureMessage raw =
  let m = T.strip raw
   in if
          | "instead of the expected hash" `T.isInfixOf` m -> FetchHashMismatch
          | "nix build failed.\n" `T.isPrefixOf` m -> NixBuildFailed
          | -- nix-build often fails via tryIOTextET: message starts with
            -- "Received ExitFailure ..." and embeds our "nix build failed." log block later
            "nix build failed." `T.isInfixOf` m ->
              NixBuildFailed
          | "ExitFailure" `T.isInfixOf` m && "nix-build" `T.isInfixOf` m && "Raw command:" `T.isInfixOf` m ->
              NixBuildFailed
          | m == "nix log failed trying to get build logs" -> NixLogFetchFailed
          | "Failed to read expected nix boolean " `T.isPrefixOf` m -> NixBooleanParseFailed
          | m == "Could not find result link." -> ResultLinkMissing
          | "build succeeded unexpectedly" `T.isInfixOf` m -> FixedOutputProbeUnexpectedSuccess
          | " according to Nix; versionComparison: " `T.isInfixOf` m -> VersionNotNewerPerNix
          | m == "An auto update branch exists with an equal or greater version" -> AutoUpdateBranchVersionBlocks
          | m == "No rewrites performed on derivation." -> NoRewritesPerformed
          | m == "Source url did not change." -> SourceUrlUnchanged
          | "cargo hashes equal; no update necessary: " `T.isPrefixOf` m -> CargoHashUnchanged
          | "deps hashes equal; no update necessary: " `T.isPrefixOf` m -> DepsHashUnchanged
          | m == "Hashes equal; no update necessary" -> FixedOutputHashUnchanged
          | m == "rev equal; no update necessary" -> RevUnchanged
          | m == "Package version did not change." -> PackageVersionUnchanged
          | m == "Update edits cause no rebuilds." -> NoRebuildsFromEdits
          | "[updateScript] Failed with exit code " `T.isPrefixOf` m ->
              if ("took longer than " `T.isInfixOf` m) && ("timed out" `T.isInfixOf` m)
                then UpdateScriptTimedOut
                else UpdateScriptFailed
          | "Too many open PRs from " `T.isPrefixOf` m -> GitHubTooManyOpenPRs
          | "There might already be an open PR for this update:\n" `T.isPrefixOf` m -> GitHubPossibleDuplicatePR
          | " is not a GitHub URL." `T.isSuffixOf` m && "GitHub: " `T.isPrefixOf` m -> GitHubUrlNotGitHub
          | "Unable to parse update: " `T.isPrefixOf` m -> UpdateInfoParseFailed
          | m == "Failed to create log directory." -> LogDirectoryCreationFailed
          | "LOGS_DIRECTORY " `T.isPrefixOf` m && " does not exist." `T.isSuffixOf` m -> LogDirectoryResolutionFailed
          | "XDG_RUNTIME_DIR " `T.isPrefixOf` m && " does not exist." `T.isSuffixOf` m -> XdgRuntimeDirResolutionFailed
          | "Temporary directory " `T.isPrefixOf` m && " does not exist." `T.isSuffixOf` m -> TemporaryDirectoryResolutionFailed
          | m == "The diff was empty after rewrites." -> DiffEmptyAfterRewrites
          | "The derivation has no 'version' attribute" `T.isPrefixOf` m -> DerivationMissingVersionAttribute
          | "Old version " `T.isPrefixOf` m && " not present in " `T.isInfixOf` m -> OldVersionMissingOnBranch
          | "Version in attr path " `T.isPrefixOf` m && " not compatible with " `T.isInfixOf` m -> AttrPathVersionPinMismatch
          | "Python package with too many package rebuilds " `T.isPrefixOf` m -> PythonRebuildLimitExceeded
          | m == "grep did not find version in file names" -> VersionCheckGrepFailed
          | "stderr did not split" `T.isInfixOf` m -> FixedOutputStderrParseFailed
          | m `S.member` skiplistReasonSet -> PolicySkiplist
          | "HttpException" `T.isInfixOf` m -> GitHubApiError
          | "StatusCodeException" `T.isInfixOf` m -> GitHubApiError
          | otherwise -> Unclassified
