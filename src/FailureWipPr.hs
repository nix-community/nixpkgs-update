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

import Control.Exception (SomeException, try)
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
  if maxN <= 0 || not (batchUpdate opts) || not (doPR opts)
    then pure ()
    else
      if outpathSkipped
        then pure ()
        else
          if not (tierA (classifyFailureMessage rawFailure))
            then pure ()
            else
              case mbRebuild of
                Nothing -> pure ()
                Just n | n > 500 -> pure ()
                Just _ -> do
                  proceed <-
                    atomicModifyIORef' wipCountRef $ \c ->
                      if c >= maxN then (c, False) else (c + 1, True)
                  when proceed $
                    void $
                      try @SomeException $
                        runExceptT $ do
                          st <- Git.statusPorcelain
                          when (T.null (T.strip st)) $
                            throwE "failure WIP PR: clean working tree"
                          let msg = "[r-ryantm] WIP failed auto-update: " <> attrPath
                          Git.commitAll msg
                          let remoteBranch = failedUpdateRemoteBranch attrPath
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
                          (prNum, url) <-
                            GH.failureWipPullRequest env title prBody commentText prHead base addComment
                          liftIO $ recordFailedWipPr attrPath prNum
                          liftIO $ do
                            emit $ "[failure-wip-pr] PR " <> tshow prNum <> " " <> url
                            hFlush stdout
