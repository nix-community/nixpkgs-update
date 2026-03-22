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
import qualified GitHub as GH
import GitHub.Data.Name (Name (..))
import NVD (withVulnDB)
import qualified Nix
import qualified Options.Applicative as O
import OurPrelude
import qualified Repology
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import qualified System.Posix.Env as P
import Text.Read (readMaybe)
import Update (cveAll, cveReport, sourceGithubAll, updatePackage)
import Utils (Options (..), UpdateEnv (..), getGithubToken, getGithubUser)

default (T.Text)

data UpdateOptions = UpdateOptions
  { pr :: Bool,
    cve :: Bool,
    nixpkgsReview :: Bool,
    outpaths :: Bool,
    attrpathOpt :: Bool,
    failureWipPrMaxCli :: Maybe Int,
    prTargetOwnerOpt :: Text,
    prTargetRepoOpt :: Text
  }

data Command
  = Update UpdateOptions Text
  | UpdateBatch UpdateOptions Text
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
    <*> O.flag False True (O.long "attrpath" <> O.help "UPDATE_INFO uses the exact attrpath.")
    <*> O.optional
          ( O.option O.auto $
              O.long "failure-wip-pr-max"
                <> O.metavar "N"
                <> O.help
                  "Max Tier-A WIP failure PRs per batch run (0=off). Overrides NIXPKGS_UPDATE_FAILURE_WIP_PR_MAX."
          )
    <*> O.strOption
          ( O.long "pr-target-owner"
              <> O.metavar "OWNER"
              <> O.value "nixos"
              <> O.showDefault
              <> O.help "GitHub owner of the target repo for pull requests (e.g. your fork's owner)."
          )
    <*> O.strOption
          ( O.long "pr-target-repo"
              <> O.metavar "REPO"
              <> O.value "nixpkgs"
              <> O.showDefault
              <> O.help "GitHub repo name for pull requests."
          )

updateParser :: O.Parser Command
updateParser =
  Update
    <$> updateOptionsParser
    <*> O.strArgument (O.metavar "UPDATE_INFO" <> O.help "update string of the form: 'pkg oldVer newVer update-page'\n\n example: 'tflint 0.15.0 0.15.1 repology.org'")

updateBatchParser :: O.Parser Command
updateBatchParser =
  UpdateBatch
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
        "update"
        (O.info (updateParser) (O.progDesc "Update one package"))
        <> O.command
          "update-batch"
          (O.info (updateBatchParser) (O.progDesc "Update one package in batch mode."))
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
  CheckVulnerable
    <$> O.strArgument (O.metavar "PRODUCT_ID")
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

resolveFailureWipPrMax :: Maybe Int -> IO Int
resolveFailureWipPrMax cli =
  case cli of
    Just n -> pure n
    Nothing -> do
      mb <- lookupEnv "NIXPKGS_UPDATE_FAILURE_WIP_PR_MAX"
      pure (fromMaybe 0 (mb >>= readMaybe))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  command <- O.execParser programInfo
  ghUser <- getGithubUser
  token <- fromMaybe "" <$> getGithubToken
  P.setEnv "GITHUB_TOKEN" (T.unpack token) True
  P.setEnv "GITHUB_API_TOKEN" (T.unpack token) True
  P.setEnv "PAGER" "" True
  case command of
    DeleteDone delete -> do
      setupNixpkgs $ GH.untagName ghUser
      deleteDone delete token ghUser
    Update UpdateOptions {pr, cve, nixpkgsReview, outpaths, attrpathOpt, failureWipPrMaxCli, prTargetOwnerOpt, prTargetRepoOpt} update -> do
      setupNixpkgs $ GH.untagName ghUser
      fw <- resolveFailureWipPrMax failureWipPrMaxCli
      updatePackage (Options pr False ghUser token cve nixpkgsReview outpaths attrpathOpt fw (N prTargetOwnerOpt) (N prTargetRepoOpt)) update
    UpdateBatch UpdateOptions {pr, cve, nixpkgsReview, outpaths, attrpathOpt, failureWipPrMaxCli, prTargetOwnerOpt, prTargetRepoOpt} update -> do
      setupNixpkgs $ GH.untagName ghUser
      fw <- resolveFailureWipPrMax failureWipPrMaxCli
      updatePackage (Options pr True ghUser token cve nixpkgsReview outpaths attrpathOpt fw (N prTargetOwnerOpt) (N prTargetRepoOpt)) update
    Version -> do
      v <- runExceptT Nix.version
      case v of
        Left t -> T.putStrLn ("error:" <> t)
        Right t -> T.putStrLn t
    UpdateVulnDB -> withVulnDB $ \_conn -> pure ()
    CheckAllVulnerable -> do
      setupNixpkgs $ GH.untagName ghUser
      updates <- T.readFile "packages-to-update.txt"
      cveAll undefined updates
    CheckVulnerable productID oldVersion newVersion -> do
      setupNixpkgs $ GH.untagName ghUser
      report <-
        cveReport
          (UpdateEnv productID oldVersion newVersion Nothing (Options False False ghUser token False False False False 0 (N "nixos") (N "nixpkgs")))
      T.putStrLn report
    SourceGithub -> do
      updates <- T.readFile "packages-to-update.txt"
      setupNixpkgs $ GH.untagName ghUser
      sourceGithubAll (Options False False ghUser token False False False False 0 (N "nixos") (N "nixpkgs")) updates
    FetchRepology -> Repology.fetch
