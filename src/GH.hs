{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module GH
  ( releaseUrl,
    GH.untagName,
    authFrom,
    authFromToken,
    checkExistingUpdatePR,
    closedAutoUpdateRefs,
    compareUrl,
    failureWipPullRequest,
    latestVersion,
    pr,
    prUpdate,
    pullNumberFromUrl,
  )
where

import Control.Applicative (liftA2, some)
import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON)
import Data.Bitraversable (bitraverse)
import Data.Char as C (isDigit)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Read (readMaybe)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import qualified Data.Vector as V
import qualified Git
import qualified GitHub as GH
import GitHub.Data.Id (mkId)
import GitHub.Data.Name (Name (..))
import GitHub.Endpoints.Issues.Labels (addLabelsToIssueR)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), responseStatus)
import Network.HTTP.Types.Status (statusCode)
import OurPrelude
import Text.Regex.Applicative.Text ((=~))
import qualified Text.Regex.Applicative.Text as RE
import Utils (UpdateEnv (..), Version, Options (..))
import qualified Utils as U

default (T.Text)

gReleaseUrl :: MonadIO m => GH.Auth -> URLParts -> ExceptT Text m Text
gReleaseUrl auth (URLParts o r t) =
  ExceptT $
    bimap (T.pack . show) (GH.getUrl . GH.releaseHtmlUrl)
      <$> liftIO (GH.github auth (GH.releaseByTagNameR o r t))

releaseUrl :: MonadIO m => UpdateEnv -> Text -> ExceptT Text m Text
releaseUrl env url = do
  urlParts <- parseURL url
  gReleaseUrl (authFrom env) urlParts

pr :: MonadIO m => UpdateEnv -> Text -> Text -> Text -> Text -> ExceptT Text m (Bool, Text)
pr env title body prHead base = do
  tryPR `catchE` \case
    -- If creating the PR returns a 422, most likely cause is that the
    -- branch was deleted, so push it again and retry once.
    GH.HTTPError (HttpExceptionRequest _ (StatusCodeException r _))
      | statusCode (responseStatus r) == 422 ->
          Git.push env >> withExceptT (T.pack . show) tryPR
    e ->
      throwE . T.pack . show $ e
  where
    targetOwner = prTargetOwner . options $ env
    targetRepo  = prTargetRepo  . options $ env
    tryPR =
      ExceptT $
        fmap ((False,) . GH.getUrl . GH.pullRequestUrl)
          <$> ( liftIO $
                  ( GH.github
                      (authFrom env)
                      ( GH.createPullRequestR
                          targetOwner
                          targetRepo
                          (GH.CreatePullRequest title body prHead base)
                      )
                  )
              )

prUpdate :: forall m. MonadIO m => UpdateEnv -> Text -> Text -> Text -> Text -> ExceptT Text m (Bool, Text)
prUpdate env title body prHead base = do
  let runRequest :: FromJSON a => GH.Request k a -> ExceptT Text m a
      runRequest = ExceptT . fmap (first (T.pack . show)) . liftIO . GH.github (authFrom env)
  let targetOwner = prTargetOwner . options $ env
      targetRepo  = prTargetRepo  . options $ env

  prs <-
    runRequest $
      GH.pullRequestsForR targetOwner targetRepo (GH.optionsHead prHead) GH.FetchAll

  case V.toList prs of
    [] -> pr env title body prHead base
    (_ : _ : _) -> throwE $ "Too many open PRs from " <> prHead
    [thePR] -> do
      let issueNum = GH.simplePullRequestNumber thePR

      _ <-
        runRequest $
          GH.updatePullRequestR targetOwner targetRepo issueNum $
            GH.EditPullRequest (Just title) Nothing Nothing Nothing Nothing

      _ <-
        runRequest $
          GH.createCommentR targetOwner targetRepo issueNum body

      return (True, GH.getUrl $ GH.simplePullRequestUrl thePR)

-- | Parse @/pull/123@ from a GitHub nixpkgs PR URL.
pullNumberFromUrl :: Text -> Maybe Int
pullNumberFromUrl u =
  case T.splitOn "/pull/" (T.takeWhile (/= '?') u) of
    (_ : numPart : _) -> readMaybe (T.unpack (T.takeWhile C.isDigit numPart))
    _ -> Nothing

issueIdFromPullNumber :: Int -> GH.Id GH.Issue
issueIdFromPullNumber = mkId (Proxy @GH.Issue) . fromIntegral

-- | Open or update a Tier-A failed-update WIP PR on NixOS/nixpkgs.
-- When @addComment@ is 'False' (same failure kind repeated past the threshold), only the title is refreshed.
failureWipPullRequest ::
  forall m.
  MonadIO m =>
  UpdateEnv ->
  Text ->
  Text ->
  Text ->
  Text ->
  Text ->
  Bool ->
  ExceptT Text m (Int, Text)
failureWipPullRequest env title prBody commentText prHead base addComment = do
  let runRequest :: FromJSON a => GH.Request k a -> ExceptT Text m a
      runRequest = ExceptT . fmap (first (T.pack . show)) . liftIO . GH.github (authFrom env)
  let targetOwner = prTargetOwner . options $ env
      targetRepo  = prTargetRepo  . options $ env

  prs <-
    runRequest $
      GH.pullRequestsForR targetOwner targetRepo (GH.optionsHead prHead) GH.FetchAll

  case V.toList prs of
    [] -> do
      prCreated <-
        runRequest $
          GH.createPullRequestR targetOwner targetRepo (GH.CreatePullRequest title prBody prHead base)
      let url = GH.getUrl (GH.pullRequestUrl prCreated)
      num <-
        maybe (throwE $ "failureWipPullRequest: no PR number in URL " <> url) pure (pullNumberFromUrl url)
      void $
        liftIO $
          try @SomeException $
            GH.github
              (authFrom env)
              (addLabelsToIssueR targetOwner targetRepo (issueIdFromPullNumber num) [N "needs-review-evidence"])
      pure (num, url)
    [_thePR] -> do
      let issueNum = GH.simplePullRequestNumber _thePR
      _ <-
        runRequest $
          GH.updatePullRequestR targetOwner targetRepo issueNum $
            GH.EditPullRequest (Just title) Nothing Nothing Nothing Nothing
      when addComment $
        void $
          runRequest $
            GH.createCommentR targetOwner targetRepo issueNum commentText
      let url = GH.getUrl $ GH.simplePullRequestUrl _thePR
      num <-
        maybe (throwE $ "failureWipPullRequest: no PR number in URL " <> url) pure (pullNumberFromUrl url)
      pure (num, url)
    _ -> throwE $ "Too many open PRs from " <> prHead

data URLParts = URLParts
  { owner :: GH.Name GH.Owner,
    repo :: GH.Name GH.Repo,
    tag :: Text
  }
  deriving (Show)

-- | Parse owner-repo-branch triplet out of URL
-- We accept URLs pointing to uploaded release assets
-- that are usually obtained with fetchurl, as well
-- as the generated archives that fetchFromGitHub downloads.
--
-- Examples:
--
-- >>> parseURLMaybe "https://github.com/blueman-project/blueman/releases/download/2.0.7/blueman-2.0.7.tar.xz"
-- Just (URLParts {owner = N "blueman-project", repo = N "blueman", tag = "2.0.7"})
--
-- >>> parseURLMaybe "https://github.com/arvidn/libtorrent/archive/libtorrent_1_1_11.tar.gz"
-- Just (URLParts {owner = N "arvidn", repo = N "libtorrent", tag = "libtorrent_1_1_11"})
--
-- >>> parseURLMaybe "https://gitlab.com/inkscape/lib2geom/-/archive/1.0/lib2geom-1.0.tar.gz"
-- Nothing
parseURLMaybe :: Text -> Maybe URLParts
parseURLMaybe url =
  let domain = RE.string "https://github.com/"
      slash = RE.sym '/'
      pathSegment = T.pack <$> some (RE.psym (/= '/'))
      extension = RE.string ".zip" <|> RE.string ".tar.gz"
      toParts n o = URLParts (N n) (N o)
      regex =
        ( toParts
            <$> (domain *> pathSegment)
            <* slash
            <*> pathSegment
            <*> (RE.string "/releases/download/" *> pathSegment)
            <* slash
            <* pathSegment
        )
          <|> ( toParts
                  <$> (domain *> pathSegment)
                  <* slash
                  <*> pathSegment
                  <*> (RE.string "/archive/" *> pathSegment)
                  <* extension
              )
   in url =~ regex

parseURL :: MonadIO m => Text -> ExceptT Text m URLParts
parseURL url =
  tryJust ("GitHub: " <> url <> " is not a GitHub URL.") (parseURLMaybe url)

compareUrl :: MonadIO m => Text -> Text -> ExceptT Text m Text
compareUrl urlOld urlNew = do
  oldParts <- parseURL urlOld
  newParts <- parseURL urlNew
  return $
    "https://github.com/"
      <> GH.untagName (owner newParts)
      <> "/"
      <> GH.untagName (repo newParts)
      <> "/compare/"
      <> tag oldParts
      <> "..."
      <> tag newParts

autoUpdateRefs :: GH.Auth -> GH.Name GH.Owner -> IO (Either Text (Vector (Text, GH.Name GH.GitCommit)))
autoUpdateRefs auth ghUser =
  GH.github auth (GH.referencesR ghUser "nixpkgs" GH.FetchAll)
    & ((fmap . fmapL) tshow)
    & ((fmap . fmapR) (fmap (liftA2 (,) (GH.gitReferenceRef >>> GH.untagName) (GH.gitReferenceObject >>> GH.gitObjectSha >>> N)) >>> V.mapMaybe (bitraverse (T.stripPrefix prefix) pure)))
  where
    prefix = "refs/heads/auto-update/"

openPRWithAutoUpdateRefFrom :: GH.Auth -> GH.Name GH.Owner -> Text -> IO (Either Text Bool)
openPRWithAutoUpdateRefFrom auth ghUser ref =
  GH.executeRequest
    auth
    ( GH.pullRequestsForR
        "nixos"
        "nixpkgs"
        (GH.optionsHead (GH.untagName ghUser <> ":" <> U.branchPrefix <> ref) <> GH.stateOpen)
        GH.FetchAll
    )
    <&> bimap (T.pack . show) (not . V.null)

commitIsOldEnoughToDelete :: GH.Auth -> GH.Name GH.Owner -> GH.Name GH.GitCommit -> IO Bool
commitIsOldEnoughToDelete auth ghUser sha = do
  now <- getCurrentTime
  let cutoff = addUTCTime (-30 * 60) now
  GH.executeRequest auth (GH.gitCommitR ghUser "nixpkgs" sha)
    <&> either (const False) ((< cutoff) . GH.gitUserDate . GH.gitCommitCommitter)

refShouldBeDeleted :: GH.Auth -> GH.Name GH.Owner -> (Text, GH.Name GH.GitCommit) -> IO Bool
refShouldBeDeleted auth ghUser (ref, sha) =
  liftA2
    (&&)
    (either (const False) not <$> openPRWithAutoUpdateRefFrom auth ghUser ref)
    (commitIsOldEnoughToDelete auth ghUser sha)

closedAutoUpdateRefs :: GH.Auth -> GH.Name GH.Owner -> IO (Either Text (Vector Text))
closedAutoUpdateRefs auth ghUser =
  runExceptT $ do
    aur :: Vector (Text, GH.Name GH.GitCommit) <- ExceptT $ GH.autoUpdateRefs auth ghUser
    ExceptT (Right . V.map fst <$> V.filterM (refShouldBeDeleted auth ghUser) aur)

authFromToken :: Text -> GH.Auth
authFromToken = GH.OAuth . T.encodeUtf8

authFrom :: UpdateEnv -> GH.Auth
authFrom = authFromToken . U.githubToken . options

checkExistingUpdatePR :: MonadIO m => UpdateEnv -> Text -> ExceptT Text m ()
checkExistingUpdatePR env attrPath = do
  searchResult <-
    ExceptT $
      liftIO $
        (GH.github (authFrom env) (GH.searchIssuesR search) GH.FetchAll)
          & fmap (first (T.pack . show))
  if T.length (openPRReport searchResult) == 0
    then return ()
    else
      throwE
        ( "There might already be an open PR for this update:\n"
            <> openPRReport searchResult
        )
  where
    targetOwner = GH.untagName . prTargetOwner . options $ env
    targetRepo  = GH.untagName . prTargetRepo  . options $ env
    title = U.prTitle env attrPath
    search = [interpolate|repo:$targetOwner/$targetRepo $title |]
    openPRReport searchResult =
      GH.searchResultResults searchResult
        & V.filter (GH.issueClosedAt >>> isNothing)
        & V.filter (GH.issuePullRequest >>> isJust)
        & fmap report
        & V.toList
        & T.unlines
    report i = "- " <> GH.issueTitle i <> "\n  " <> tshow (GH.issueUrl i)

latestVersion :: MonadIO m => UpdateEnv -> Text -> ExceptT Text m Version
latestVersion env url = do
  urlParts <- parseURL url
  r <-
    fmapLT tshow $
      ExceptT $
        liftIO $
          GH.executeRequest (authFrom env) $
            GH.latestReleaseR (owner urlParts) (repo urlParts)
  return $ T.dropWhile (\c -> c == 'v' || c == 'V') (GH.releaseTagName r)
