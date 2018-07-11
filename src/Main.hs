{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Applicative ((<**>), (<|>))
import Control.Exception
import Data.Maybe (isJust)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import DeleteMerged (deleteMerged)
import qualified Options.Applicative as Opt
import System.Posix.Env (getEnv)
import Update (updateAll)
import Utils (Options(..))

default (T.Text)

data Mode
  = Update
  | DeleteMerged

modeParser :: Opt.Parser Mode
modeParser =
  Opt.flag'
    Update
    (Opt.long "update" <> Opt.help "Update packages (default mode)") <|>
  Opt.flag'
    DeleteMerged
    (Opt.long "delete-merged" <>
     Opt.help "Delete branches that were already merged")

programInfo :: Opt.ParserInfo Mode
programInfo =
  Opt.info
    (modeParser <**> Opt.helper)
    (Opt.fullDesc <> Opt.progDesc "Update packages in nixpkgs repository" <>
     Opt.header "nixpkgs-update")

makeOptions :: IO Options
makeOptions = do
  dryRun <- isJust <$> getEnv "DRY_RUN"
  githubToken <- T.strip <$> T.readFile "github_token.txt"
  return $ Options dryRun "~/.nixpkgs-update" githubToken

main :: IO ()
main = do
  mode <- Opt.execParser programInfo
  options <- makeOptions
  case mode of
    DeleteMerged -> deleteMerged options
    Update -> updateAll options
