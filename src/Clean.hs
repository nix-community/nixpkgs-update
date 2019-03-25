{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Clean
  ( fixSrcUrl
  ) where

import OurPrelude

import Control.Applicative (some)
import qualified Data.Text as T
import qualified File
import qualified Shell
import Shelly hiding (FilePath, whenM)
import qualified Text.Regex.Applicative.Text as RE
import Text.Regex.Applicative.Text (RE', (=~))
import Utils (UpdateEnv(..), Version)

default (T.Text)

-- | Construct regex: ${version}[^/]+\.(tar|zip)
archiveRegex :: Version -> RE' ()
archiveRegex version =
  (\_ _ _ _ -> ()) <$> RE.string version <*> some (RE.psym (/= '/')) <*>
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
    (Shell.succeeded $
     cmd
       "grep"
       "-q"
       ("name = \"" <> newDerivationName <> "\"")
       (T.pack derivationFile)) $
    -- Separate name and version
   do
    let newName = name <> "-${version}"
    File.replace newDerivationName newName derivationFile
    _ <-
      cmd "grep" "-q" ("name = \"" <> newName <> "\"") (T.pack derivationFile)
    _ <-
      cmd
        "sed"
        "-i"
        ("s|^\\([ ]*\\)\\(name = \"" <> name <>
         "-${version}\";\\)|\\1\\2\\n\\1version = \"" <>
         newVersion updateEnv <>
         "\";|")
        (T.pack derivationFile)
    cmd
      "grep"
      "-q"
      ("version = \"" <> newVersion updateEnv <> "\";")
      (T.pack derivationFile)
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
             isNothing (url =~ archiveRegex (newVersion updateEnv))) &
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
      _ <-
        lift $
        cmd "grep" "-q" ("url = \"" <> newUrl <> "\";") (T.pack derivationFile)
      whenM
        (lift $
         Shell.succeeded $
         cmd "grep" "-q" ("url = \"" <> newUrl <> "\";") (T.pack derivationFile)) $ do
        hash <-
          lift $
          Shell.canFail $ cmd "nix-prefetch-url" "-A" (attrPath <> ".src")
        succeedT hash
      return ()
  case forResult of
    Right hash -> return hash
    _ -> cmd "false"
