{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Clean
  ( fixSrcUrl
  ) where

import Control.Applicative ((<|>), some)
import Control.Error
import Control.Monad (forM_)
import Control.Monad.Trans
import Data.Function ((&))
import Data.Maybe (isNothing)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Prelude hiding (FilePath, break)
import Shelly
import qualified Text.Regex.Applicative as RE
import Text.Regex.Applicative (RE, (=~))
import Utils (Version, canFail, setupNixpkgs, succeded)

default (T.Text)

-- | Construct regex: ${version}[^/]+\.(tar|zip)
archiveRegex :: Version -> RE Char ()
archiveRegex version =
  (\_ _ _ _ -> ()) <$> RE.string (T.unpack version) <*> some (RE.psym (/= '/')) <*>
  RE.sym '.' <*>
  (RE.string "tar" <|> RE.string "zip")

fixSrcUrl :: Text -> Version -> Version -> FilePath -> Text -> Text -> Sh Text
fixSrcUrl packageName oldVersion newVersion derivationFile attrPath oldSrcUrl = do
  nixpkgsPath <- setupNixpkgs
  oldDerivationName <-
    T.strip <$>
    cmd "nix" "eval" "-f" nixpkgsPath "--raw" ("pkgs." <> attrPath <> ".name")
  let newDerivationName = T.replace oldVersion newVersion oldDerivationName
  name <-
    T.strip <$>
    cmd
      "nix"
      "eval"
      "--raw"
      ("(let pkgs = import ./. {}; in (builtins.parseDrvName pkgs." <> attrPath <>
       ".name).name)")
  whenM
    (succeded $
     cmd "grep" "-q" ("name = \"" <> newDerivationName <> "\"") derivationFile) $
    -- Separate name and version
   do
    cmd
      "sed"
      "-i"
      ("s|" <> newDerivationName <> "|" <> name <> "-${version}|")
      derivationFile
    cmd "grep" "-q" ("name = \"" <> name <> "-${version}\"") derivationFile
    cmd
      "sed"
      "-i"
      ("s|^\\([ ]*\\)\\(name = \"" <> name <>
       "-${version}\";\\)|\\1\\2\n\\1version = \"" <>
       newVersion <>
       "\";|")
      derivationFile
    cmd "grep" "-q" ("version = \"" <> newVersion <> "\";") derivationFile
  downloads <-
    cmd "curl" ("https://repology.org/api/v1/metapackage/" <> packageName) -|-
    cmd "jq" ".[].downloads | select(values) | .[]"
  let filteredDownloads =
        downloads & T.lines &
        filter
          (\url ->
             newVersion `T.isInfixOf` url &&
             isNothing (T.unpack url =~ archiveRegex newVersion)) &
        map (T.replace "\"" "")
  forResult <-
    runExceptT $
    runExceptRT $
    forM_ filteredDownloads $ \downloadUrl -> do
      let oldUrl =
            T.replace
              oldVersion
              "${version}"
              (T.replace oldDerivationName "${name}" oldSrcUrl)
      let newUrl =
            T.replace
              newVersion
              "${version}"
              (T.replace newDerivationName "${name}" downloadUrl)
      lift $
        cmd "sed" "-i" ("s|" <> oldUrl <> "|" <> newUrl <> "|") derivationFile
      lift $ cmd "grep" "-q" ("url = \"" <> newUrl <> "\";") derivationFile
      whenM
        (lift $
         succeded $
         cmd "grep" "-q" ("url = \"" <> newUrl <> "\";") derivationFile) $ do
        hash <-
          lift $ canFail $ cmd "nix-prefetch-url" "-A" (attrPath <> ".src")
        succeedT hash
      return ()
  case forResult of
    Right hash -> return hash
    _ -> cmd "false"
