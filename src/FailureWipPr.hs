{-# LANGUAGE OverloadedStrings #-}

-- | Tier-A (@NixBuildFailed@, @UpdateScriptFailed@) failed-update WIP PRs to
-- NixOS/nixpkgs. Pushes @HEAD@ to @auto-update/@\<sanitized-attr\>@-failed@ on the
-- bot fork, then opens or updates a PR with @WIP:@ in the title. Rate-limited
-- per batch run via 'Options.failureWipPrMax'. Requires @needs-review-evidence@
-- label on nixpkgs; label application failures are ignored.
module FailureWipPr
  ( tryTierAFailureWipPr,
  )
where

import Control.Exception (SomeException, displayException, try)
import Data.IORef (IORef, atomicModifyIORef')
import qualified Data.Text as T
import FailureDb (recordFailedWipPr)
import FailureKind (FailureKind (..), classifyFailureMessage)
import qualified GH
import qualified Git
import qualified Nix
import OurPrelude
import System.IO (hFlush, stdout)
import Utils (Options (..), UpdateEnv (..), branchPrefix)

tierA :: FailureKind -> Bool
tierA NixBuildFailed = True
tierA UpdateScriptFailed = True
tierA _ = False

sanitizeRemoteSegment :: Text -> Text
sanitizeRemoteSegment =
  T.map $ \c ->
    if c == '/' || c == '\\' || c == ':' || c == ' ' || c == '*' || c == '?' || c == '[' || c == '^' || c == '~'
      then '-'
      else c

failedUpdateRemoteBranch :: Text -> Text
failedUpdateRemoteBranch attrPath =
  branchPrefix <> sanitizeRemoteSegment attrPath <> "-failed"

wipTrace :: (Text -> IO ()) -> Text -> IO ()
wipTrace emit msg = do
  emit $ "[failure-wip-pr] " <> msg
  hFlush stdout

-- | After a Tier-A failure with a dirty tree, push and open/update WIP PR.
tryTierAFailureWipPr ::
  IORef Int ->
  (Text -> IO ()) ->
  UpdateEnv ->
  Text ->
  Text ->
  Bool ->
  Maybe Int ->
  Int ->
  IO ()
tryTierAFailureWipPr wipCountRef emit env attrPath rawFailure outpathSkipped mbRebuild consecutiveFailures = do
  let opts = options env
      maxN = failureWipPrMax opts
      kind = classifyFailureMessage rawFailure
      kindTxt = tshow kind
  wipTrace emit $ "start attr=" <> attrPath <> " classified=" <> kindTxt <> " failure_wip_pr_max=" <> tshow maxN <> " outpath_skipped=" <> tshow outpathSkipped <> " mbRebuild=" <> tshow mbRebuild <> " consecutive_failures=" <> tshow consecutiveFailures
  if maxN <= 0 || not (batchUpdate opts) || not (doPR opts)
    then
      wipTrace emit $
        "skip: guards (need failure_wip_pr_max>0, batch_update, pr). maxN="
          <> tshow maxN
          <> " batch="
          <> tshow (batchUpdate opts)
          <> " pr="
          <> tshow (doPR opts)
    else
      if outpathSkipped
        then wipTrace emit "skip: outpath_skipped (package on skiplist for outpath calc)"
        else
          if not (tierA kind)
            then wipTrace emit $ "skip: not Tier-A (only NixBuildFailed, UpdateScriptFailed). got " <> kindTxt
            else
              case mbRebuild of
                Just n | n > 500 -> wipTrace emit $ "skip: rebuild estimate " <> tshow n <> " > 500"
                _ -> do
                  proceed <-
                    atomicModifyIORef' wipCountRef $ \c ->
                      if c >= maxN then (c, False) else (c + 1, True)
                  if not proceed
                    then wipTrace emit $ "skip: WIP PR budget exhausted (" <> tshow maxN <> " per batch run)"
                    else do
                      wipTrace emit "proceed: checking git status (need dirty tree in worktree cwd)..."
                      exOrEt <-
                        try @SomeException $
                          runExceptT $ do
                            st <- Git.statusPorcelain
                            liftIO $ wipTrace emit $ "git status --porcelain (first 500 chars): " <> T.take 500 st
                            when (T.null (T.strip st)) $
                              throwE "failure WIP PR: clean working tree"
                            let msg = "[r-ryantm] WIP failed auto-update: " <> attrPath
                            liftIO $ wipTrace emit "committing broken tree..."
                            Git.commitAll msg
                            let remoteBranch = failedUpdateRemoteBranch attrPath
                            liftIO $ wipTrace emit $ "pushing HEAD to origin as " <> remoteBranch <> " ..."
                            Git.pushHeadToRemoteBranch remoteBranch env
                            ghUser <- pure $ GH.untagName (githubUser opts)
                            let prHead = ghUser <> ":" <> remoteBranch
                                base = "master"
                                oV = oldVersion env
                                nV = newVersion env
                                title =
                                  "WIP: failed auto-update: "
                                    <> attrPath
                                    <> " ("
                                    <> oV
                                    <> " -> "
                                    <> nV
                                    <> ")"
                            maint <-
                              liftIO $
                                fmap (either (const "(could not list maintainers)") id) $
                                  runExceptT (Nix.getMaintainers attrPath)
                            let logsBaseUrl = "https://nixpkgs-update-logs.nix-community.org/"
                                excerpt = T.take 8000 rawFailure
                                prBody =
                                  T.unlines
                                    [ "## nixpkgs-update failed run (Tier A)",
                                      "",
                                      "This PR was opened automatically with the **broken tree** from a failed batch run so maintainers can reproduce and fix.",
                                      "",
                                      "- **attr_path**: `" <> attrPath <> "`",
                                      "- **old -> new**: `" <> oV <> "` -> `" <> nV <> "`",
                                      "- **Maintainers**: " <> maint,
                                      "",
                                      "### Logs",
                                      "",
                                      "Browse [public batch logs](" <> logsBaseUrl <> ") for this attr path and date.",
                                      "",
                                      "### Failure excerpt",
                                      "",
                                      "```",
                                      excerpt,
                                      "```",
                                      "",
                                      "### Evidence before removing WIP",
                                      "",
                                      "Before you remove **WIP:** from the title, run nixpkgs-review on a branch that contains your fix and **paste** the report output in a **comment**.",
                                      "",
                                      "```bash",
                                      "nix-shell -p nixpkgs-review --run \"nixpkgs-review rev <your-fix-rev>\"",
                                      "```",
                                      "",
                                      "Label `needs-review-evidence` should exist on nixpkgs.",
                                      "",
                                      "---",
                                      "*Automation: [nixpkgs-update](https://github.com/nix-community/nixpkgs-update)*"
                                    ]
                                commentText =
                                  "Automated batch run excerpt (streak > 3: title-only refresh):\n\n```\n"
                                    <> excerpt
                                    <> "\n```"
                                addComment = consecutiveFailures <= 3
                            liftIO $ wipTrace emit $ "opening PR base=" <> base <> " head=" <> prHead
                            (prNum, url) <-
                              GH.failureWipPullRequest env title prBody commentText prHead base addComment
                            liftIO $ recordFailedWipPr attrPath prNum
                            liftIO $ wipTrace emit $ "done PR " <> tshow prNum <> " " <> url
                      case exOrEt of
                        Left ex ->
                          wipTrace emit $ "abort: exception: " <> T.pack (displayException ex)
                        Right (Left e) ->
                          wipTrace emit $ "abort: " <> e
                        Right (Right ()) -> pure ()
