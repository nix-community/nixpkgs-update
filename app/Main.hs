{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Control.Applicative ((<**>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import DeleteMerged (deleteDone)
import Git
import NVD (withVulnDB)
import qualified Nix
import qualified Options.Applicative as O
import OurPrelude
import qualified Repology
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import qualified System.Posix.Env as P
import Update (cveAll, cveReport, sourceGithubAll, updateAll, updatePackage)
import Utils (Options (..), UpdateEnv (..), getGithubToken, getGithubUser)

default (T.Text)

data UpdateOptions = UpdateOptions
  { pr :: Bool,
    cve :: Bool,
    nixpkgsReview :: Bool,
    outpaths :: Bool
  }

data Command
  = UpdateList UpdateOptions
  | Update UpdateOptions Text
  | DeleteDone Bool
  | Version
  | UpdateVulnDB
  | CheckAllVulnerable
  | SourceGithub
  | FetchRepology
  | CheckVulnerable Text Text Text

updateOptionsParser :: O.Parser UpdateOptions
updateOptionsParser =
  UpdateOptions
    <$> O.flag False True (O.long "pr" <> O.help "Make a pull request using Hub.")
    <*> O.flag False True (O.long "cve" <> O.help "Make a CVE vulnerability report.")
    <*> O.flag False True (O.long "nixpkgs-review" <> O.help "Runs nixpkgs-review on update commit rev")
    <*> O.flag False True (O.long "outpaths" <> O.help "Calculate outpaths to determine the branch to target")

updateParser :: O.Parser Command
updateParser =
  Update
    <$> updateOptionsParser
    <*> O.strArgument (O.metavar "UPDATE_INFO" <> O.help "update string of the form: 'pkg oldVer newVer update-page'\n\n example: 'tflint 0.15.0 0.15.1 repology.org'")

deleteDoneParser :: O.Parser Command
deleteDoneParser =
  DeleteDone
    <$> O.flag False True (O.long "delete" <> O.help "Actually delete the done branches. Otherwise just prints the branches to delete.")

commandParser :: O.Parser Command
commandParser =
  O.hsubparser
    ( O.command
        "update-list"
        (O.info (UpdateList <$> updateOptionsParser) (O.progDesc "Update a list of packages"))
        <> O.command
          "update"
          (O.info (updateParser) (O.progDesc "Update one package"))
        <> O.command
          "delete-done"
          ( O.info
              deleteDoneParser
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
  ghUser <- getGithubUser
  token <- getGithubToken <|> undefined
  P.setEnv "GITHUB_TOKEN" (T.unpack token) True
  P.setEnv "PAGER" "" True
  case command of
    DeleteDone delete -> do
      Git.setupNixpkgs token
      deleteDone delete token ghUser
    UpdateList UpdateOptions {pr, cve, nixpkgsReview, outpaths} -> do
      updates <- T.readFile "packages-to-update.txt"
      Git.setupNixpkgs token
      updateAll (Options pr True ghUser token cve nixpkgsReview outpaths) updates
    Update UpdateOptions {pr, cve, nixpkgsReview} update -> do
      Git.setupNixpkgs token
      result <- updatePackage (Options pr False ghUser token cve nixpkgsReview False) update
      case result of
        Left e -> T.putStrLn e
        Right () -> T.putStrLn "Done."
    Version -> do
      v <- runExceptT Nix.version
      case v of
        Left t -> T.putStrLn ("error:" <> t)
        Right t -> T.putStrLn t
    UpdateVulnDB -> withVulnDB $ \_conn -> pure ()
    CheckAllVulnerable -> do
      setupNixpkgs undefined
      updates <- T.readFile "packages-to-update.txt"
      cveAll undefined updates
    CheckVulnerable productID oldVersion newVersion -> do
      setupNixpkgs undefined
      report <-
        cveReport
          (UpdateEnv productID oldVersion newVersion Nothing (Options False False ghUser token False False False))
      T.putStrLn report
    SourceGithub -> do
      updates <- T.readFile "packages-to-update.txt"
      setupNixpkgs token
      sourceGithubAll (Options False False ghUser token False False False) updates
    FetchRepology -> Repology.fetch
