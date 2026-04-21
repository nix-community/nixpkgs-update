{-# LANGUAGE OverloadedStrings #-}

module Forge
  ( compareUrl,
    forgeName,
    URLParts (..),
    Forge (..),
    parseURLMaybe,
  )
where

import Control.Applicative (some, (<|>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.Applicative.Text ((=~))
import qualified Text.Regex.Applicative.Text as RE

data Forge = GitHub | GitLab | Gitea | SourceHut
  deriving (Show, Eq)

data URLParts = URLParts
  { forge :: Forge,
    baseUrl :: Text,
    owner :: Text,
    repo :: Text,
    tag :: Text
  }
  deriving (Show, Eq)

forgeName :: Forge -> Text
forgeName GitHub = "GitHub"
forgeName GitLab = "GitLab"
forgeName Gitea = "Gitea"
forgeName SourceHut = "SourceHut"

-- | Build a compare URL appropriate for the given forge.
--
-- >>> buildCompareUrl (URLParts GitHub "https://github.com" "owner" "repo" "v1.0") (URLParts GitHub "https://github.com" "owner" "repo" "v1.1")
-- "https://github.com/owner/repo/compare/v1.0...v1.1"
--
-- >>> buildCompareUrl (URLParts GitLab "https://gitlab.com" "owner" "repo" "v1.0") (URLParts GitLab "https://gitlab.com" "owner" "repo" "v1.1")
-- "https://gitlab.com/owner/repo/-/compare/v1.0...v1.1"
--
-- >>> buildCompareUrl (URLParts Gitea "https://codeberg.org" "owner" "repo" "v1.0") (URLParts Gitea "https://codeberg.org" "owner" "repo" "v1.1")
-- "https://codeberg.org/owner/repo/compare/v1.0...v1.1"
--
-- >>> buildCompareUrl (URLParts SourceHut "https://git.sr.ht" "~owner" "repo" "v1.0") (URLParts SourceHut "https://git.sr.ht" "~owner" "repo" "v1.1")
-- "https://git.sr.ht/~owner/repo/log/v1.0..v1.1"
buildCompareUrl :: URLParts -> URLParts -> Text
buildCompareUrl old new =
  case forge new of
    GitHub ->
      baseUrl new
        <> "/"
        <> owner new
        <> "/"
        <> repo new
        <> "/compare/"
        <> tag old
        <> "..."
        <> tag new
    GitLab ->
      baseUrl new
        <> "/"
        <> owner new
        <> "/"
        <> repo new
        <> "/-/compare/"
        <> tag old
        <> "..."
        <> tag new
    Gitea ->
      baseUrl new
        <> "/"
        <> owner new
        <> "/"
        <> repo new
        <> "/compare/"
        <> tag old
        <> "..."
        <> tag new
    SourceHut ->
      baseUrl new
        <> "/"
        <> owner new
        <> "/"
        <> repo new
        <> "/log/"
        <> tag old
        <> ".."
        <> tag new

-- | Parse a source URL into forge-specific parts.
--
-- GitHub release download:
-- >>> parseURLMaybe "https://github.com/blueman-project/blueman/releases/download/2.0.7/blueman-2.0.7.tar.xz"
-- Just (URLParts {forge = GitHub, baseUrl = "https://github.com", owner = "blueman-project", repo = "blueman", tag = "2.0.7"})
--
-- GitHub archive:
-- >>> parseURLMaybe "https://github.com/arvidn/libtorrent/archive/libtorrent_1_1_11.tar.gz"
-- Just (URLParts {forge = GitHub, baseUrl = "https://github.com", owner = "arvidn", repo = "libtorrent", tag = "libtorrent_1_1_11"})
--
-- GitLab archive:
-- >>> parseURLMaybe "https://gitlab.com/inkscape/lib2geom/-/archive/1.0/lib2geom-1.0.tar.gz"
-- Just (URLParts {forge = GitLab, baseUrl = "https://gitlab.com", owner = "inkscape", repo = "lib2geom", tag = "1.0"})
--
-- Self-hosted GitLab:
-- >>> parseURLMaybe "https://gitlab.gnome.org/GNOME/glib/-/archive/2.80.0/glib-2.80.0.tar.gz"
-- Just (URLParts {forge = GitLab, baseUrl = "https://gitlab.gnome.org", owner = "GNOME", repo = "glib", tag = "2.80.0"})
--
-- Codeberg archive:
-- >>> parseURLMaybe "https://codeberg.org/dnkl/foot/archive/1.16.1.tar.gz"
-- Just (URLParts {forge = Gitea, baseUrl = "https://codeberg.org", owner = "dnkl", repo = "foot", tag = "1.16.1"})
--
-- SourceHut archive:
-- >>> parseURLMaybe "https://git.sr.ht/~sircmpwn/hare/archive/0.24.0.tar.gz"
-- Just (URLParts {forge = SourceHut, baseUrl = "https://git.sr.ht", owner = "~sircmpwn", repo = "hare", tag = "0.24.0"})
--
-- Non-forge URL:
-- >>> parseURLMaybe "https://example.com/foo-1.0.tar.gz"
-- Nothing
parseURLMaybe :: Text -> Maybe URLParts
parseURLMaybe url =
  parseGitHub url
    <|> parseGitLab url
    <|> parseCodeberg url
    <|> parseSourceHut url

-- | Try to generate a compare URL between two source URLs.
--
-- >>> compareUrl "https://github.com/owner/repo/archive/v1.0.tar.gz" "https://github.com/owner/repo/archive/v1.1.tar.gz"
-- Just "https://github.com/owner/repo/compare/v1.0...v1.1"
--
-- >>> compareUrl "https://gitlab.com/owner/repo/-/archive/v1.0/repo-v1.0.tar.gz" "https://gitlab.com/owner/repo/-/archive/v1.1/repo-v1.1.tar.gz"
-- Just "https://gitlab.com/owner/repo/-/compare/v1.0...v1.1"
--
-- >>> compareUrl "https://example.com/foo-1.0.tar.gz" "https://example.com/foo-1.1.tar.gz"
-- Nothing
compareUrl :: Text -> Text -> Maybe Text
compareUrl urlOld urlNew = do
  oldParts <- parseURLMaybe urlOld
  newParts <- parseURLMaybe urlNew
  Just (buildCompareUrl oldParts newParts)

-- Internal parsers

slash :: RE.RE Char Char
slash = RE.sym '/'

pathSegment :: RE.RE Char Text
pathSegment = T.pack <$> some (RE.psym (/= '/'))

extension :: RE.RE Char Text
extension = RE.string ".zip" <|> RE.string ".tar.gz" <|> RE.string ".tar.xz" <|> RE.string ".tar.bz2"

parseGitHub :: Text -> Maybe URLParts
parseGitHub url =
  let domain = RE.string "https://github.com/"
      toParts o r t = URLParts GitHub "https://github.com" o r t
      releaseDownload =
        toParts
          <$> (domain *> pathSegment)
          <* slash
          <*> pathSegment
          <*> (RE.string "/releases/download/" *> pathSegment)
          <* slash
          <* pathSegment
      archive =
        toParts
          <$> (domain *> pathSegment)
          <* slash
          <*> pathSegment
          <*> (RE.string "/archive/" *> pathSegment)
          <* extension
   in url =~ (releaseDownload <|> archive)

parseGitLab :: Text -> Maybe URLParts
parseGitLab url =
  let -- Match https://<anything-containing-gitlab>/<owner>/<repo>/-/archive/<tag>/<filename>
      scheme = RE.string "https://"
      -- Match domain containing "gitlab" (e.g. gitlab.com, gitlab.gnome.org, gitlab.freedesktop.org)
      domainChar = RE.psym (\c -> c /= '/')
      gitlabDomain = T.pack <$> some domainChar
      toParts domain o r t = URLParts GitLab ("https://" <> domain) o r t
      regex =
        toParts
          <$> (scheme *> gitlabDomain)
          <* slash
          <*> pathSegment
          <* slash
          <*> pathSegment
          <*> (RE.string "/-/archive/" *> pathSegment)
          <* slash
          <* pathSegment
   in url =~ regex

parseCodeberg :: Text -> Maybe URLParts
parseCodeberg url =
  let domain = RE.string "https://codeberg.org/"
      toParts o r t = URLParts Gitea "https://codeberg.org" o r t
      releaseDownload =
        toParts
          <$> (domain *> pathSegment)
          <* slash
          <*> pathSegment
          <*> (RE.string "/releases/download/" *> pathSegment)
          <* slash
          <* pathSegment
      archive =
        toParts
          <$> (domain *> pathSegment)
          <* slash
          <*> pathSegment
          <*> (RE.string "/archive/" *> pathSegment)
          <* extension
   in url =~ (releaseDownload <|> archive)

parseSourceHut :: Text -> Maybe URLParts
parseSourceHut url =
  let domain = RE.string "https://git.sr.ht/"
      -- SourceHut owners always start with ~
      srhtOwner = T.pack <$> ((:) <$> RE.sym '~' <*> some (RE.psym (/= '/')))
      toParts o r t = URLParts SourceHut "https://git.sr.ht" o r t
      archive =
        toParts
          <$> (domain *> srhtOwner)
          <* slash
          <*> pathSegment
          <*> (RE.string "/archive/" *> pathSegment)
          <* extension
   in url =~ archive
