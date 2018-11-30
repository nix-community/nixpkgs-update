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
import qualified File
import Prelude hiding (FilePath)
import Shelly
import qualified Text.Regex.Applicative as RE
import Text.Regex.Applicative (RE, (=~))
import Utils (UpdateEnv(..), Version, canFail, setupNixpkgs, succeded)

default (T.Text)

-- | Construct regex: ${version}[^/]+\.(tar|zip)
archiveRegex :: Version -> RE Char ()
archiveRegex version =
  (\_ _ _ _ -> ()) <$> RE.string (T.unpack version) <*> some (RE.psym (/= '/')) <*>
  RE.sym '.' <*>
  (RE.string "tar" <|> RE.string "zip")

fixSrcUrl :: UpdateEnv -> FilePath -> Text -> Text -> Sh Text
fixSrcUrl updateEnv derivationFile attrPath oldSrcUrl = do
  oldDerivationName <-
    T.strip <$>
    cmd "nix" "eval" "-f" "." "--raw" ("pkgs." <> attrPath <> ".name")
  let newDerivationName =
        T.replace
          (oldVersion updateEnv)
          (newVersion updateEnv)
          oldDerivationName
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
    let newName = name <> "-${version}"
    File.replace newDerivationName newName derivationFile
    cmd "grep" "-q" ("name = \"" <> newName <> "\"") derivationFile
    cmd
      "sed"
      "-i"
      ("s|^\\([ ]*\\)\\(name = \"" <> name <>
       "-${version}\";\\)|\\1\\2\n\\1version = \"" <>
       newVersion updateEnv <>
       "\";|")
      derivationFile
    cmd
      "grep"
      "-q"
      ("version = \"" <> newVersion updateEnv <> "\";")
      derivationFile
  -- Obtain download URLs from repology
  -- TODO: use repology-api package
  downloads <-
    cmd
      "curl"
      "-s"
      ("https://repology.org/api/v1/metapackage/" <> packageName updateEnv) -|-
    cmd "jq" ".[].downloads | select(values) | .[]"
  let filteredDownloads =
        downloads & T.lines &
        filter
          (\url ->
             newVersion updateEnv `T.isInfixOf` url &&
             isNothing (T.unpack url =~ archiveRegex (newVersion updateEnv))) &
        map (T.replace "\"" "")
  forResult <-
    runExceptT $
    runExceptRT $
    forM_ filteredDownloads $ \downloadUrl -> do
      let oldUrl =
            T.replace
              (oldVersion updateEnv)
              "${version}"
              (T.replace oldDerivationName "${name}" oldSrcUrl)
      let newUrl =
            T.replace
              (newVersion updateEnv)
              "${version}"
              (T.replace newDerivationName "${name}" downloadUrl)
      lift $ File.replace oldUrl newUrl derivationFile
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
