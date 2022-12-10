{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module GH
  ( releaseUrl,
    GH.untagName,
    authFromToken,
    checkExistingUpdatePR,
    closedAutoUpdateRefs,
    compareUrl,
    latestVersion,
    pr,
    prUpdate,
  )
where

import Control.Applicative (some)
import Data.Aeson (FromJSON)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified GitHub as GH
import GitHub.Data.Name (Name (..))
import OurPrelude
import Text.Regex.Applicative.Text ((=~))
import qualified Text.Regex.Applicative.Text as RE
import Utils (UpdateEnv (..), Version)
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
  ExceptT $
    bimap (T.pack . show) ((False, ) . GH.getUrl . GH.pullRequestUrl)
      <$> ( liftIO $
              ( GH.github
                  (authFrom env)
                  ( GH.createPullRequestR
                      (N "nixos")
                      (N "nixpkgs")
                      (GH.CreatePullRequest title body prHead base)
                  )
              )
          )

prUpdate :: forall m. MonadIO m => UpdateEnv -> Text -> Text -> Text -> Text -> ExceptT Text m (Bool, Text)
prUpdate env title body prHead base = do
  let runRequest :: FromJSON a => GH.Request k a -> ExceptT Text m a
      runRequest = ExceptT . fmap (first (T.pack . show)) . liftIO . GH.github (authFrom env)
  let inNixpkgs f = f (N "nixos") (N "nixpkgs")

  prs <- runRequest $
    inNixpkgs GH.pullRequestsForR (GH.optionsHead prHead) GH.FetchAll

  case V.toList prs of
    [] -> pr env title body prHead base

    (_:_:_) -> throwE $ "Too many open PRs from " <> prHead

    [thePR] -> do
      let withExistingPR :: (GH.Name GH.Owner -> GH.Name GH.Repo -> GH.IssueNumber -> a) -> a
          withExistingPR f = inNixpkgs f (GH.simplePullRequestNumber thePR)

      _ <- runRequest $
        withExistingPR GH.updatePullRequestR $
          GH.EditPullRequest (Just title) Nothing Nothing Nothing Nothing

      _ <- runRequest $
        withExistingPR GH.createCommentR body

      return (True, GH.getUrl $ GH.simplePullRequestUrl thePR)

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
        ( toParts <$> (domain *> pathSegment) <* slash <*> pathSegment
            <*> (RE.string "/releases/download/" *> pathSegment)
            <* slash
            <* pathSegment
        )
          <|> ( toParts <$> (domain *> pathSegment) <* slash <*> pathSegment
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

autoUpdateRefs :: GH.Auth -> GH.Name GH.Owner -> IO (Either Text (Vector Text))
autoUpdateRefs auth ghUser =
  GH.github auth (GH.referencesR ghUser "nixpkgs" GH.FetchAll)
    & ((fmap . fmapL) tshow)
    & ((fmap . fmapR) (fmap (GH.gitReferenceRef >>> GH.untagName) >>> V.mapMaybe (T.stripPrefix prefix)))
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
    & fmap (first (T.pack . show) >>> second (not . V.null))

refShouldBeDeleted :: GH.Auth -> GH.Name GH.Owner -> Text -> IO Bool
refShouldBeDeleted auth ghUser ref =
  not . either (const True) id
    <$> openPRWithAutoUpdateRefFrom auth ghUser ref

closedAutoUpdateRefs :: GH.Auth -> GH.Name GH.Owner -> IO (Either Text (Vector Text))
closedAutoUpdateRefs auth ghUser =
  runExceptT $ do
    aur :: Vector Text <- ExceptT $ GH.autoUpdateRefs auth ghUser
    ExceptT (Right <$> V.filterM (refShouldBeDeleted auth ghUser) aur)

authFromToken :: Text -> GH.Auth
authFromToken = GH.OAuth . T.encodeUtf8

authFrom :: UpdateEnv -> GH.Auth
authFrom = authFromToken . U.githubToken . options

checkExistingUpdatePR :: MonadIO m => UpdateEnv -> Text -> Text -> ExceptT Text m ()
checkExistingUpdatePR env attrPath srcUrl = do
  searchResult <-
    ExceptT $
      liftIO $
        GH.github (authFrom env) (GH.searchIssuesR search)
          & fmap (first (T.pack . show))
  when (T.length (openPRReport searchResult) /= 0)
    (throwE
    ( "There might already be an open PR for this update:\n"
      <> openPRReport searchResult))

  srcUrlSearchResult <-
    ExceptT $
      liftIO $
        GH.github (authFrom env) (GH.searchIssuesR srcUrlSearch)
          & fmap (first (T.pack . show))

  when (srcUrl /= T.empty && T.length (openPRReport srcUrlSearchResult) /= 0)
    (throwE
    ( "There might already be an open PR for this update:\n"
      <> openPRReport searchResult))

  return ()
  where
    title = U.prTitle env attrPath
    search = [interpolate|repo:nixos/nixpkgs $title |]
    srcUrlSearch = [interpolate|new src url: $srcUrl |]
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
