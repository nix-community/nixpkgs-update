{-# LANGUAGE OverloadedStrings #-}

module GitHub
  ( releaseUrl
  ) where

import Control.Error
import Data.Bifunctor (bimap)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import GitHub.Auth (Auth(..))
import GitHub.Data.Definitions (Owner)
import GitHub.Data.Name (Name(..))
import GitHub.Data.Releases (releaseBody, releaseHtmlUrl)
import GitHub.Data.Repos (Repo)
import GitHub.Data.URL (getUrl)
import GitHub.Endpoints.Repos.Releases (releaseByTagName)

gReleaseUrl :: Name Owner -> Name Repo -> Text -> IO (Either Text Text)
gReleaseUrl owner repo tag =
  bimap (T.pack . show) (getUrl . releaseHtmlUrl) <$> releaseByTagName owner repo tag

releaseUrl :: Text -> IO (Either Text Text)
releaseUrl url =
  runExceptT $ do
    tryAssert
      ("GitHub: " <> url <> " is not a GitHub URL.")
      ("https://github.com/" `T.isPrefixOf` url)
    let parts = T.splitOn "/" url
    owner <- N <$> tryAt ("GitHub: owner part missing from " <> url) parts 3
    repo <- N <$> tryAt ("GitHub: repo part missing from " <> url) parts 4
    revPart <- tryAt ("GitHub: rev part missing from " <> url) parts 6
    rev <-
      tryJust
        ("GitHub: rev part missing .tar.gz suffix " <> url)
        (T.stripSuffix ".tar.gz" revPart)
    ExceptT $ gReleaseUrl owner repo rev
