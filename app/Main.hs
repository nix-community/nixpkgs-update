{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Control.Applicative ((<**>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import DeleteMerged (deleteDone)
import NVD (withVulnDB)
import qualified Nix
import qualified Options.Applicative as O
import OurPrelude
import qualified Repology
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import qualified System.Posix.Env as P
import Update (cveAll, cveReport, sourceGithubAll, updateAll)
import Utils (Options (..), UpdateEnv (..), getGithubToken, setupNixpkgs)

default (T.Text)

data UpdateOptions
  = UpdateOptions
      { dry :: Bool,
        cachix :: Bool,
        additionalUpdates :: Text,
        outpaths :: Bool
      }

data Command
  = UpdateList UpdateOptions
  | Update UpdateOptions
  | DeleteDone
  | Version
  | UpdateVulnDB
  | CheckAllVulnerable
  | SourceGithub
  | FetchRepology
  | CheckVulnerable Text Text Text

updateOptionsParser :: O.Parser UpdateOptions
updateOptionsParser =
  UpdateOptions
    <$> O.switch
      ( O.long "dry-run"
          <> O.help
            "Do everything except actually pushing the updates to the remote repository"
      )
    <*> O.flag False True (O.long "cachix" <> O.help "Push changes to Cachix")
    <*> O.strOption (O.long "additional-updates" <> O.help "A string of updates formatted the same way as packages-to-update.txt" <> O.value "")
    <*> O.flag False True (O.long "outpaths" <> O.help "Calculate outpaths to determine the branch to target")

commandParser :: O.Parser Command
commandParser =
  O.hsubparser
    ( O.command
        "update-list"
        (O.info (UpdateList <$> updateOptionsParser) (O.progDesc "Update a list of packages"))
        <> O.command
          "update"
          (O.info (Update <$> updateOptionsParser) (O.progDesc "Update packages"))
        <> O.command
          "delete-done"
          ( O.info
              (pure DeleteDone)
              (O.progDesc "Deletes branches from PRs that were merged or closed")
          )
        <> O.command
          "version"
          ( O.info
              (pure Version)
              ( O.progDesc
                  "Displays version information for nixpkgs-update and dependencies"
              )
          )
        <> O.command
          "update-vulnerability-db"
          ( O.info
              (pure UpdateVulnDB)
              (O.progDesc "Updates the vulnerability database")
          )
        <> O.command
          "check-vulnerable"
          (O.info checkVulnerable (O.progDesc "checks if something is vulnerable"))
        <> O.command
          "check-all-vulnerable"
          ( O.info
              (pure CheckAllVulnerable)
              (O.progDesc "checks all packages to update for vulnerabilities")
          )
        <> O.command
          "source-github"
          (O.info (pure SourceGithub) (O.progDesc "looks for updates on GitHub"))
        <> O.command
          "fetch-repology"
          (O.info (pure FetchRepology) (O.progDesc "fetches update from Repology and prints them to stdout"))
    )

checkVulnerable :: O.Parser Command
checkVulnerable =
  CheckVulnerable <$> O.strArgument (O.metavar "PRODUCT_ID")
    <*> O.strArgument (O.metavar "OLD_VERSION")
    <*> O.strArgument (O.metavar "NEW_VERSION")

programInfo :: O.ParserInfo Command
programInfo =
  O.info
    (commandParser <**> O.helper)
    ( O.fullDesc
        <> O.progDesc "Update packages in the Nixpkgs repository"
        <> O.header "nixpkgs-update"
    )

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  command <- O.execParser programInfo
  case command of
    DeleteDone -> do
      token <- getGithubToken
      setupNixpkgs token
      P.setEnv "GITHUB_TOKEN" (T.unpack token) True
      deleteDone token
    UpdateList UpdateOptions {dry, cachix, additionalUpdates, outpaths} -> do
      token <- getGithubToken
      updates <- T.readFile "packages-to-update.txt"
      setupNixpkgs token
      P.setEnv "PAGER" "" True
      P.setEnv "GITHUB_TOKEN" (T.unpack token) True
      updateAll (Options dry token cachix outpaths) (updates <> "\n" <> additionalUpdates)
    Update UpdateOptions {dry, cachix, additionalUpdates, outpaths} -> do
      token <- getGithubToken
      setupNixpkgs token
      P.setEnv "PAGER" "" True
      P.setEnv "GITHUB_TOKEN" (T.unpack token) True
      updateAll (Options dry token cachix outpaths) additionalUpdates
    Version -> do
      v <- runExceptT Nix.version
      case v of
        Left t -> T.putStrLn ("error:" <> t)
        Right t -> T.putStrLn t
    UpdateVulnDB -> withVulnDB $ \_conn -> pure ()
    CheckAllVulnerable -> do
      setupNixpkgs undefined
      updates <- T.readFile "packages-to-update.txt"
      cveAll (Options undefined undefined undefined undefined) updates
    CheckVulnerable productID oldVersion newVersion -> do
      setupNixpkgs undefined
      report <-
        cveReport
          (UpdateEnv productID oldVersion newVersion Nothing (Options False undefined False False))
      T.putStrLn report
    SourceGithub -> do
      token <- getGithubToken
      updates <- T.readFile "packages-to-update.txt"
      setupNixpkgs token
      P.setEnv "GITHUB_TOKEN" (T.unpack token) True
      sourceGithubAll (Options False token False False) updates
    FetchRepology -> Repology.fetch
