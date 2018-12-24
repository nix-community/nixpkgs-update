{-# LANGUAGE ExtendedDefaultRules #-}
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
  ) where

import OurPrelude

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import GitHub
import GitHub.Data.Name (Name(..), untagName)
import GitHub.Endpoints.GitData.References (references')
import GitHub.Endpoints.Repos.Releases (releaseByTagName)
import Shelly hiding (tag)
import Utils

gReleaseUrl :: MonadIO m => URLParts -> ExceptT Text m Text
gReleaseUrl (URLParts o r t) =
  ExceptT $
  bimap (T.pack . show) (getUrl . releaseHtmlUrl) <$>
  (liftIO $ releaseByTagName o r t)

releaseUrl :: MonadIO m => Text -> ExceptT Text m Text
releaseUrl url = do
  urlParts <- parseURL url
  gReleaseUrl urlParts

pr :: Text -> Text -> Sh ()
pr base = cmd "hub" "pull-request" "-b" base "-m"

data URLParts = URLParts
  { owner :: Name Owner
  , repo :: Name Repo
  , tag :: Text
  }

parseURL :: MonadIO m => Text -> ExceptT Text m URLParts
parseURL url = do
  tryAssert
    ("GitHub: " <> url <> " is not a GitHub URL.")
    ("https://github.com/" `T.isPrefixOf` url)
  let parts = T.splitOn "/" url
  o <- N <$> tryAt ("GitHub: owner part missing from " <> url) parts 3
  r <- N <$> tryAt ("GitHub: repo part missing from " <> url) parts 4
  tagPart <- tryAt ("GitHub: tag part missing from " <> url) parts 6
  t <-
    tryJust
      ("GitHub: tag part missing .tar.gz suffix " <> url)
      (T.stripSuffix ".tar.gz" tagPart)
  return $ URLParts o r t

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
       (optionsHead ("r-ryantm:auto-update/" <> ref) <> stateOpen)
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

openPullRequests :: Options -> IO (Either Text (Vector SimplePullRequest))
openPullRequests o =
  executeRequest
    (OAuth (T.encodeUtf8 (githubToken o)))
    (pullRequestsForR "nixos" "nixpkgs" stateOpen FetchAll) &
  fmap (first (T.pack . show))

openAutoUpdatePR :: UpdateEnv -> Vector SimplePullRequest -> Bool
openAutoUpdatePR updateEnv openPRs = openPRs & (V.find isThisPkg >>> isJust)
  where
    isThisPkg simplePullRequest =
      let title = simplePullRequestTitle simplePullRequest
          titleHasName = (packageName updateEnv <> ":") `T.isPrefixOf` title
          titleHasNewVersion = newVersion updateEnv `T.isSuffixOf` title
       in titleHasName && titleHasNewVersion
