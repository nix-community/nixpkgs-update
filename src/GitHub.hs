{-# LANGUAGE OverloadedStrings #-}

module GitHub
  ( changelog
  ) where

import Control.Error
import Data.Bifunctor (bimap)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import GitHub.Auth (Auth(..))
import GitHub.Data.Definitions (Owner)
import GitHub.Data.Name (Name(..))
import GitHub.Data.Releases (releaseBody)
import GitHub.Data.Repos (Repo)
import GitHub.Endpoints.Repos.Releases (releaseByTagName)

gchangelog :: Name Owner -> Name Repo -> Text -> IO (Either Text Text)
gchangelog owner repo tag =
  bimap (T.pack . show) releaseBody <$> releaseByTagName owner repo tag

changelog :: Text -> IO (Either Text Text)
changelog url =
  runExceptT $ do
    tryAssert
      ("Changelog: " <> url <> " is not a GitHub URL.")
      ("https://github.com/" `T.isPrefixOf` url)
    let parts = T.splitOn "/" url
    owner <- N <$> tryAt ("Changelog: owner part missing from " <> url) parts 3
    repo <- N <$> tryAt ("Changelog: repo part missing from " <> url) parts 4
    revPart <- tryAt ("Changelog: rev part missing from " <> url) parts 6
    rev <-
      tryJust
        ("Changelog: rev part missing .tar.gz suffix " <> url)
        (T.stripSuffix ".tar.gz" revPart)
    ExceptT $ gchangelog owner repo rev
