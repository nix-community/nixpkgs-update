{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import OurPrelude

import Control.Applicative ((<**>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import DeleteMerged (deleteDone)
import NVD (withVulnDB)
import qualified Nix
import qualified Options.Applicative as O
import System.Posix.Env (setEnv)
import Update (cveAll, cveReport, updateAll)
import Utils (Options(..), UpdateEnv(..), setupNixpkgs)

default (T.Text)

newtype UpdateOptions =
  UpdateOptions
    { dry :: Bool
    }

data Command
  = Update UpdateOptions
  | DeleteDone
  | Version
  | UpdateVulnDB
  | CheckAllVulnerable
  | CheckVulnerable Text Text Text

updateOptionsParser :: O.Parser Command
updateOptionsParser =
  Update . UpdateOptions <$>
  O.switch
    (O.long "dry-run" <>
     O.help
       "Do everything except actually pushing the updates to the remote repository")

commandParser :: O.Parser Command
commandParser =
  O.hsubparser
    (O.command
       "update"
       (O.info updateOptionsParser (O.progDesc "Update packages")) <>
     O.command
       "delete-done"
       (O.info
          (pure DeleteDone)
          (O.progDesc "Deletes branches from PRs that were merged or closed")) <>
     O.command
       "version"
       (O.info
          (pure Version)
          (O.progDesc
             "Displays version information for nixpkgs-update and dependencies")) <>
     O.command
       "update-vulnerability-db"
       (O.info
          (pure UpdateVulnDB)
          (O.progDesc "Updates the vulnerability database")) <>
     O.command
       "check-vulnerable"
       (O.info checkVulnerable (O.progDesc "checks if something is vulnerable")) <>
     O.command
       "check-all-vulnerable"
       (O.info
          (pure CheckAllVulnerable)
          (O.progDesc "checks all packages to update for vulnerabilities")))

checkVulnerable :: O.Parser Command
checkVulnerable =
  CheckVulnerable <$> O.strArgument (O.metavar "PRODUCT_ID") <*>
  O.strArgument (O.metavar "OLD_VERSION") <*>
  O.strArgument (O.metavar "NEW_VERSION")

programInfo :: O.ParserInfo Command
programInfo =
  O.info
    (commandParser <**> O.helper)
    (O.fullDesc <>
     O.progDesc "Update packages in the Nixpkgs repository" <>
     O.header "nixpkgs-update")

getGithubToken :: IO Text
getGithubToken = T.strip <$> T.readFile "github_token.txt"

main :: IO ()
main = do
  command <- O.execParser programInfo
  case command of
    DeleteDone -> do
      token <- getGithubToken
      setupNixpkgs token
      setEnv "GITHUB_TOKEN" (T.unpack token) True
      deleteDone token
    Update UpdateOptions {dry} -> do
      token <- getGithubToken
      updates <- T.readFile "packages-to-update.txt"
      setupNixpkgs token
      setEnv "PAGER" "" True
      setEnv "GITHUB_TOKEN" (T.unpack token) True
      setEnv "GC_INITIAL_HEAP_SIZE" "10g" True
      updateAll (Options dry token) updates
    Version -> do
      v <- runExceptT Nix.version
      case v of
        Left t -> T.putStrLn ("error:" <> t)
        Right t -> T.putStrLn t
    UpdateVulnDB -> withVulnDB $ \_conn -> pure ()
    CheckAllVulnerable -> do
      updates <- T.readFile "packages-to-update.txt"
      cveAll (Options undefined undefined) updates
    CheckVulnerable productID oldVersion newVersion -> do
      report <-
        cveReport
          (UpdateEnv productID oldVersion newVersion (Options False undefined))
      T.putStrLn report
