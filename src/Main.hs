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
import qualified Options.Applicative as Opt
import System.Posix.Env (setEnv)
import Update (updateAll)
import Utils (Options(..), setupNixpkgs)

default (T.Text)

data Mode
  = Update
  | DeleteDone

data Arguments =
  Arguments
    { mode :: Mode
    , dry :: Bool
    }

modeParser :: Opt.Parser Mode
modeParser =
  Opt.flag'
    Update
    (Opt.long "update" <> Opt.help "Update packages (default mode)") <|>
  Opt.flag'
    DeleteDone
    (Opt.long "delete-done" <>
     Opt.help "Delete branches from PRs that were merged or closed")

argumentParser :: Opt.Parser Arguments
argumentParser =
  Arguments <$> modeParser <*>
  Opt.switch
    (Opt.long "dry-run" <>
     Opt.help
       "Do everything except actually pushing the updates to the remote repository")

programInfo :: Opt.ParserInfo Arguments
programInfo =
  Opt.info
    (argumentParser <**> Opt.helper)
    (Opt.fullDesc <> Opt.progDesc "Update packages in nixpkgs repository" <>
     Opt.header "nixpkgs-update")

makeOptions :: Arguments -> IO Options
makeOptions Arguments {dry} = do
  token <- T.strip <$> T.readFile "github_token.txt"
  return $ Options dry token

main :: IO ()
main = do
  arguments@Arguments {mode} <- Opt.execParser programInfo
  options <- makeOptions arguments
  updates <- T.readFile "packages-to-update.txt"
  setupNixpkgs options
  setEnv "PAGER" "" True
  setEnv "GITHUB_TOKEN" (T.unpack (githubToken options)) True
  setEnv "GC_INITIAL_HEAP_SIZE" "10g" True
  case mode of
    DeleteDone -> deleteDone options
    Update -> updateAll options updates
