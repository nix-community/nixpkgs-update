{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Clean (fixSrcUrl) where

import Control.Applicative ((<|>), some)
import Data.Function ((&))
import qualified Data.Text as T
import Control.Monad (forM_)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Utils (Version, succeded, setupNixpkgs)
import Data.Semigroup ((<>))
import Shelly
import qualified Text.Regex.Applicative as RE
import Text.Regex.Applicative (RE, (=~))

default (T.Text)

-- | Construct regex: ${version}[^/]+\.(tar|zip)
archiveRegex :: Version -> RE Char ()
archiveRegex version = (\_ _ _ _ -> ()) <$> RE.string (T.unpack version) <*> some (RE.psym (/= '/')) <*> RE.sym '.' <*> (RE.string "tar" <|> RE.string "zip")


fixSrcUrl :: Text -> Version -> Version -> Text -> Text -> Text -> Sh Text
fixSrcUrl packageName oldVersion newVersion derivationFile attrPath oldSrcUrl = do
    nixpkgsPath <- setupNixpkgs

    oldDerivationName <- cmd "nix" "eval" "-f" nixpkgsPath "--raw" ("pkgs." <> attrPath <> ".name")
    let newDerivationName = T.replace oldVersion newVersion oldDerivationName
    name <- cmd "nix" "eval" "--raw" ("(let pkgs = import ./. {}; in (builtins.parseDrvName pkgs." <> attrPath <> ".name).name)")

    whenM (succeded $ cmd "grep" "-q" ("name = \"" <> newDerivationName <> "\"") derivationFile) $ do
        -- Separate name and version
        cmd "sed" "-i" ("s|" <> newDerivationName <> "|" <> name <> "-${version}|") derivationFile
        cmd "grep" "-q" ("name = \"" <> name <> "-${version}\"") derivationFile
        cmd "sed" "-i" ("s|^\\([ ]*\\)\\(name = \"" <> name <> "-${version}\";\\)|\\1\\2\n\\1version = \"" <> newVersion <> "\";|") derivationFile
        cmd "grep" "-q" ("version = \"" <> newVersion <> "\";") derivationFile

    downloads <- cmd "curl" ("https://repology.org/api/v1/metapackage/" <> packageName) -|- cmd "jq" ".[].downloads | select(values) | .[]"
    let filteredDownloads = downloads & T.lines & filter (\url -> newVersion `T.isInfixOf` url && isNothing (T.unpack url =~ archiveRegex newVersion)) & map (T.replace "\"" "")

    forM_ filteredDownloads $ \downloadUrl -> do
        let oldUrl = T.replace oldVersion "${version}" (T.replace oldDerivationName "${name}" oldSrcUrl)
        let newUrl = T.replace newVersion "${version}" (T.replace newDerivationName "${name}" downloadUrl)
        cmd "sed" "-i" ("s|" <> oldUrl <> "|" <> newUrl <> "|") derivationFile
        cmd "grep" "-q" ("url = \"" <> newUrl <> "\";") derivationFile

        whenM (succeded $ cmd "grep" "-q" ("url = \"" <> newUrl <> "\";") derivationFile) $ do
            whenM (succeded $ cmd "nix-prefetch-url" "-A" (attrPath <> ".src")) (exit 0)

    exit 1
