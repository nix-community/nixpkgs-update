{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module GH
  ( GH.releaseUrl,
    compareUrl,
    pr,
    closedAutoUpdateRefs,
    openPullRequests,
    openAutoUpdatePR,
    checkExistingUpdatePR,
    latestVersion,
    authFromToken,
  )
where

import Control.Applicative (some)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import GitHub
import GitHub.Data.Name (Name (..))
import OurPrelude
import qualified Text.Regex.Applicative.Text as RE
import Text.Regex.Applicative.Text ((=~))
import Utils (UpdateEnv (..), Version)
import qualified Utils as U

default (T.Text)

gReleaseUrl :: MonadIO m => Auth -> URLParts -> ExceptT Text m Text
gReleaseUrl auth (URLParts o r t) =
  ExceptT $
    bimap (T.pack . show) (getUrl . releaseHtmlUrl)
      <$> liftIO (github auth (releaseByTagNameR o r t))

releaseUrl :: MonadIO m => UpdateEnv -> Text -> ExceptT Text m Text
releaseUrl env url = do
  urlParts <- parseURL url
  gReleaseUrl (authFrom env) urlParts

pr :: MonadIO m => UpdateEnv -> Text -> Text -> Text -> Text -> ExceptT Text m Text
pr env title body prHead base = do
  ExceptT $
    bimap (T.pack . show) (getUrl . pullRequestUrl)
      <$> (liftIO $ (github (authFrom env)
         (createPullRequestR (N "nixos") (N "nixpkgs")
          (CreatePullRequest title body prHead base))))

data URLParts
  = URLParts
      { owner :: Name Owner,
        repo :: Name Repo,
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
      <> untagName (owner newParts)
      <> "/"
      <> untagName (repo newParts)
      <> "/compare/"
      <> tag oldParts
      <> "..."
      <> tag newParts

--deleteDoneBranches :: IO ()
--deleteDoneBranches = do
-- (OAuth (T.encodeUtf8 githubToken))
autoUpdateRefs :: Auth -> IO (Either Text (Vector Text))
autoUpdateRefs auth =
  github auth (referencesR "r-ryantm" "nixpkgs" FetchAll)
    & fmap
      ( first (T.pack . show)
          >>> second (fmap gitReferenceRef >>> V.mapMaybe (T.stripPrefix prefix))
      )
  where
    prefix = "refs/heads/auto-update/"

openPRWithAutoUpdateRefFromRRyanTM :: Auth -> Text -> IO (Either Text Bool)
openPRWithAutoUpdateRefFromRRyanTM auth ref =
  executeRequest
    auth
    ( pullRequestsForR
        "nixos"
        "nixpkgs"
        (optionsHead ("r-ryantm:" <> U.branchPrefix <> ref) <> stateOpen)
        FetchAll
    )
    & fmap (first (T.pack . show) >>> second (not . V.null))

refShouldBeDeleted :: Auth -> Text -> IO Bool
refShouldBeDeleted auth ref =
  not . either (const True) id
    <$> openPRWithAutoUpdateRefFromRRyanTM auth ref

closedAutoUpdateRefs :: Auth -> IO (Either Text (Vector Text))
closedAutoUpdateRefs auth =
  runExceptT $ do
    aur :: Vector Text <- ExceptT $ autoUpdateRefs auth
    ExceptT (Right <$> V.filterM (refShouldBeDeleted auth) aur)

-- This is too slow
openPullRequests :: Text -> IO (Either Text (Vector SimplePullRequest))
openPullRequests githubToken =
  executeRequest
    (OAuth (T.encodeUtf8 githubToken))
    (pullRequestsForR "nixos" "nixpkgs" stateOpen FetchAll)
    & fmap (first (T.pack . show))

openAutoUpdatePR :: UpdateEnv -> Vector SimplePullRequest -> Bool
openAutoUpdatePR updateEnv oprs = oprs & (V.find isThisPkg >>> isJust)
  where
    isThisPkg simplePullRequest =
      let title = simplePullRequestTitle simplePullRequest
          titleHasName = (packageName updateEnv <> ":") `T.isPrefixOf` title
          titleHasNewVersion = newVersion updateEnv `T.isSuffixOf` title
       in titleHasName && titleHasNewVersion

authFromToken :: Text -> Auth
authFromToken = OAuth . T.encodeUtf8

authFrom :: UpdateEnv -> Auth
authFrom = authFromToken . U.githubToken . options

checkExistingUpdatePR :: MonadIO m => UpdateEnv -> Text -> ExceptT Text m ()
checkExistingUpdatePR env attrPath = do
  searchResult <-
    ExceptT
      $ liftIO
      $ github (authFrom env) (searchIssuesR search)
        & fmap (first (T.pack . show))
  if T.length (openPRReport searchResult) == 0
    then return ()
    else
      throwE
        ( "There might already be an open PR for this update:\n"
            <> openPRReport searchResult
        )
  where
    title = U.prTitle env attrPath
    search = [interpolate|repo:nixos/nixpkgs $title |]
    openPRReport searchResult =
      searchResultResults searchResult & V.filter (issueClosedAt >>> isNothing)
        & fmap report
        & V.toList
        & T.unlines
    report i = "- " <> issueTitle i <> "\n  " <> tshow (issueUrl i)

latestVersion :: MonadIO m => UpdateEnv -> Text -> ExceptT Text m Version
latestVersion env url = do
  urlParts <- parseURL url
  r <-
    fmapLT tshow $ ExceptT
      $ liftIO
      $ executeRequest (authFrom env)
      $ latestReleaseR (owner urlParts) (repo urlParts)
  return $ T.dropWhile (\c -> c == 'v' || c == 'V') (releaseTagName r)
