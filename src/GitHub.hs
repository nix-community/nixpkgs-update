{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module GitHub
  ( releaseUrl
  , compareUrl
  , pr
  ) where

import Control.Error
import Data.Bifunctor (bimap)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import GitHub.Auth (Auth(..))
import GitHub.Data.Definitions (Owner)
import GitHub.Data.Name (Name(..), untagName)
import GitHub.Data.Releases (releaseBody, releaseHtmlUrl)
import GitHub.Data.Repos (Repo)
import GitHub.Data.URL (getUrl)
import GitHub.Endpoints.Repos.Releases (releaseByTagName)
import Shelly hiding (tag)

gReleaseUrl :: URLParts -> IO (Either Text Text)
gReleaseUrl (URLParts owner repo tag) =
  bimap (T.pack . show) (getUrl . releaseHtmlUrl) <$>
  releaseByTagName owner repo tag

releaseUrl :: Text -> IO (Either Text Text)
releaseUrl url =
  runExceptT $ do
    urlParts <- ExceptT $ parseURL url
    ExceptT $ gReleaseUrl urlParts

pr :: Text -> Sh ()
pr = cmd "hub" "pull-request" "-m"

data URLParts = URLParts
  { owner :: Name Owner
  , repo :: Name Repo
  , tag :: Text
  }

parseURL :: Text -> IO (Either Text URLParts)
parseURL url =
  runExceptT $ do
    tryAssert
      ("GitHub: " <> url <> " is not a GitHub URL.")
      ("https://github.com/" `T.isPrefixOf` url)
    let parts = T.splitOn "/" url
    owner <- N <$> tryAt ("GitHub: owner part missing from " <> url) parts 3
    repo <- N <$> tryAt ("GitHub: repo part missing from " <> url) parts 4
    tagPart <- tryAt ("GitHub: tag part missing from " <> url) parts 6
    tag <-
      tryJust
        ("GitHub: tag part missing .tar.gz suffix " <> url)
        (T.stripSuffix ".tar.gz" tagPart)
    return $ URLParts owner repo tag

compareUrl :: Text -> Text -> IO (Either Text Text)
compareUrl urlOld urlNew =
  runExceptT $ do
    oldParts <- ExceptT $ parseURL urlOld
    newParts <- ExceptT $ parseURL urlNew
    return $ "https://github.com/" <> untagName (owner newParts) <> "/" <> untagName (repo newParts) <> "/compare/" <> tag oldParts <> "..." <> tag newParts
