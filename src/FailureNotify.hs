{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Optional GitHub issues for update failures. When @NIXPKGS_UPDATE_FAILURE_ISSUES=1@
-- and @GITHUB_TOKEN@ is set (via 'Utils.Options'), opens or comments on a deduplicated
-- issue in the repo given by @NIXPKGS_UPDATE_FAILURE_ISSUE_REPO@ (default
-- @nix-community/nixpkgs-update@). API errors are swallowed so batch runs never fail
-- on notification.
module FailureNotify
  ( maybeNotifyFailureIssue,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (unless, void)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import FailureKind (classifyFailureMessage, failureKindCode)
import qualified GH
import qualified GitHub as GitHub
import GitHub.Data.Issues (NewIssue (..))
import GitHub.Data.Name (Name (..))
import GitHub.Endpoints.Issues (createIssueR, newIssue)
import GitHub.Endpoints.Issues.Comments (createCommentR)
import OurPrelude (interpolate)
import System.Environment (lookupEnv)
import Utils (UpdateEnv (..), githubToken, options)

defaultFailureIssueRepo :: Text
defaultFailureIssueRepo = "nix-community/nixpkgs-update"

failureIssueTitle :: Text -> Text
failureIssueTitle ap = "[nixpkgs-update failure] " <> ap

maxIssueBodyChars :: Int
maxIssueBodyChars = 58000

maxCommentBodyChars :: Int
maxCommentBodyChars = 8000

parseOwnerRepo :: Text -> Maybe (Name GitHub.Owner, Name GitHub.Repo)
parseOwnerRepo t = case T.splitOn "/" t of
  [o, r]
    | not (T.null o),
      not (T.null r) ->
        Just (N o, N r)
  _ -> Nothing

stripDoubleQuotes :: Text -> Text
stripDoubleQuotes = T.replace "\"" ""

issueSearchQuery :: Text -> Text -> Text
issueSearchQuery ownerRepo titleForSearch =
  let titleQ = stripDoubleQuotes titleForSearch
   in [interpolate|repo:$ownerRepo is:issue is:open in:title "$titleQ"|]

buildIssueBody :: Text -> Text -> Text -> Text
buildIssueBody attrPath kindCode excerpt =
  let logsHint =
        "Public batch logs (when published): https://nixpkgs-update-logs.nix-community.org/\n"
          <> "Search or browse for attr path: `"
          <> attrPath
          <> "`.\n"
   in T.take maxIssueBodyChars $
        T.unlines
          [ "Automated nixpkgs-update run recorded a failure for this attr path.",
            "",
            "- **attr_path**: `" <> attrPath <> "`",
            "- **failure_kind**: `" <> kindCode <> "`",
            "",
            "### Excerpt",
            "",
            "```",
            excerpt,
            "```",
            "",
            "### Maintainer follow-up",
            "",
            "If you investigate, running `nixpkgs-review` on a branch that contains your fix "
              <> "and pasting the summary here helps reviewers and future triage.",
            "",
            logsHint
          ]

buildCommentBody :: Text -> Text -> Text -> IO Text
buildCommentBody attrPath kindCode excerpt = do
  t <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t
  pure $
    T.take maxCommentBodyChars $
      T.unlines
        [ "Another failure for `" <> attrPath <> "` at " <> T.pack ts <> ".",
          "",
          "- **failure_kind**: `" <> kindCode <> "`",
          "",
          "```",
          excerpt,
          "```"
        ]

-- | When @NIXPKGS_UPDATE_FAILURE_ISSUES=1@, create or comment a tracking issue.
maybeNotifyFailureIssue :: UpdateEnv -> Text -> Text -> IO ()
maybeNotifyFailureIssue env attrPath rawMsg = do
  enabled <- lookupEnv "NIXPKGS_UPDATE_FAILURE_ISSUES"
  case enabled of
    Just "1" -> void $ try @SomeException $ runNotify
    _ -> pure ()
  where
    runNotify :: IO ()
    runNotify = do
      let tok = githubToken (options env)
      unless (T.null tok) $ do
        repoText <- maybe defaultFailureIssueRepo T.pack <$> lookupEnv "NIXPKGS_UPDATE_FAILURE_ISSUE_REPO"
        case parseOwnerRepo repoText of
          Nothing -> pure ()
          Just (ownerN, repoN) -> do
            let auth = GH.authFrom env
                kindCode = failureKindCode (classifyFailureMessage rawMsg)
                excerpt = T.take 12000 rawMsg
                titleTxt = failureIssueTitle attrPath
                search = issueSearchQuery repoText titleTxt
            sr <- GitHub.github auth (GitHub.searchIssuesR search) GitHub.FetchAll
            case sr of
              Left _ -> pure ()
              Right res -> do
                let wanted = titleTxt
                    openMatches =
                      V.filter
                        ( \i ->
                            GitHub.issueTitle i == wanted
                              && isNothing (GitHub.issuePullRequest i)
                              && isNothing (GitHub.issueClosedAt i)
                        )
                        (GitHub.searchResultResults res)
                if V.null openMatches
                  then do
                    let ni =
                          (newIssue titleTxt)
                            { newIssueBody = Just (buildIssueBody attrPath kindCode excerpt)
                            }
                    void $ GitHub.github auth (createIssueR ownerN repoN ni)
                  else do
                    let num = GitHub.issueNumber (V.head openMatches)
                    cbody <- buildCommentBody attrPath kindCode excerpt
                    void $ GitHub.github auth (createCommentR ownerN repoN num cbody)
