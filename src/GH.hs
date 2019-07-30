{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module GH
  ( GH.releaseUrl
  , compareUrl
  , pr
  , closedAutoUpdateRefs
  , openPullRequests
  , openAutoUpdatePR
  , checkExistingUpdatePR
  ) where

import OurPrelude

import Control.Applicative (some)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import GitHub
import GitHub.Data.Name (Name(..), untagName)
import GitHub.Endpoints.GitData.References (references')
import GitHub.Endpoints.Repos.Releases (releaseByTagName)
import GitHub.Endpoints.Search (searchIssues')
import qualified Text.Regex.Applicative.Text as RE
import Text.Regex.Applicative.Text ((=~))
import Utils

default (T.Text)

gReleaseUrl :: MonadIO m => URLParts -> ExceptT Text m Text
gReleaseUrl (URLParts o r t) =
  ExceptT $
  bimap (T.pack . show) (getUrl . releaseHtmlUrl) <$>
  liftIO (releaseByTagName o r t)

releaseUrl :: MonadIO m => Text -> ExceptT Text m Text
releaseUrl url = do
  urlParts <- parseURL url
  gReleaseUrl urlParts

pr :: MonadIO m => Text -> Text -> m ()
pr base msg =
  runProcess_ $
  proc "hub" ["pull-request", "-b", T.unpack base, "-m", T.unpack msg]

data URLParts =
  URLParts
    { owner :: Name Owner
    , repo :: Name Repo
    , tag :: Text
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
        (toParts <$> (domain *> pathSegment) <* slash <*> pathSegment <*>
         (RE.string "/releases/download/" *> pathSegment) <*
         slash <*
         pathSegment) <|>
        (toParts <$> (domain *> pathSegment) <* slash <*> pathSegment <*>
         (RE.string "/archive/" *> pathSegment) <*
         extension)
   in url =~ regex

parseURL :: MonadIO m => Text -> ExceptT Text m URLParts
parseURL url =
  tryJust ("GitHub: " <> url <> " is not a GitHub URL.") (parseURLMaybe url)

compareUrl :: MonadIO m => Text -> Text -> ExceptT Text m Text
compareUrl urlOld urlNew = do
  oldParts <- parseURL urlOld
  newParts <- parseURL urlNew
  return $
    "https://github.com/" <> untagName (owner newParts) <> "/" <>
    untagName (repo newParts) <>
    "/compare/" <>
    tag oldParts <>
    "..." <>
    tag newParts

--deleteDoneBranches :: IO ()
--deleteDoneBranches = do
autoUpdateRefs :: Options -> IO (Either Text (Vector Text))
autoUpdateRefs o =
  references' (Just (OAuth (T.encodeUtf8 (githubToken o)))) "r-ryantm" "nixpkgs" &
  fmap
    (first (T.pack . show) >>>
     second (fmap gitReferenceRef >>> V.mapMaybe (T.stripPrefix prefix)))
  where
    prefix = "refs/heads/auto-update/"

openPRWithAutoUpdateRefFromRRyanTM :: Options -> Text -> IO (Either Text Bool)
openPRWithAutoUpdateRefFromRRyanTM o ref =
  executeRequest
    (OAuth (T.encodeUtf8 (githubToken o)))
    (pullRequestsForR
       "nixos"
       "nixpkgs"
       (optionsHead ("r-ryantm:" <> branchPrefix <> ref) <> stateOpen)
       FetchAll) &
  fmap (first (T.pack . show) >>> second (not . V.null))

refShouldBeDeleted :: Options -> Text -> IO Bool
refShouldBeDeleted o ref =
  not . either (const True) id <$> openPRWithAutoUpdateRefFromRRyanTM o ref

closedAutoUpdateRefs :: Options -> IO (Either Text (Vector Text))
closedAutoUpdateRefs o =
  runExceptT $ do
    aur :: Vector Text <- ExceptT $ autoUpdateRefs o
    ExceptT (Right <$> V.filterM (refShouldBeDeleted o) aur)

-- This is too slow
openPullRequests :: Options -> IO (Either Text (Vector SimplePullRequest))
openPullRequests o =
  executeRequest
    (OAuth (T.encodeUtf8 (githubToken o)))
    (pullRequestsForR "nixos" "nixpkgs" stateOpen FetchAll) &
  fmap (first (T.pack . show))

openAutoUpdatePR :: UpdateEnv -> Vector SimplePullRequest -> Bool
openAutoUpdatePR updateEnv oprs = oprs & (V.find isThisPkg >>> isJust)
  where
    isThisPkg simplePullRequest =
      let title = simplePullRequestTitle simplePullRequest
          titleHasName = (packageName updateEnv <> ":") `T.isPrefixOf` title
          titleHasNewVersion = newVersion updateEnv `T.isSuffixOf` title
       in titleHasName && titleHasNewVersion

checkExistingUpdatePR :: MonadIO m => UpdateEnv -> Text -> ExceptT Text m ()
checkExistingUpdatePR ue attrPath = do
  searchResult <-
    ExceptT $
    liftIO $
    searchIssues'
      (Just (OAuth (T.encodeUtf8 (githubToken (options ue)))))
      search &
    fmap (first (T.pack . show))
  when
    (anyOpen searchResult)
    (throwE "There is already an open PR for this update")
  where
    title = prTitle ue attrPath
    search = [interpolate|repo:nixos/nixpkgs $title |]
    anyOpen searchResult =
      any (issueClosedAt >>> isNothing) (searchResultResults searchResult)
