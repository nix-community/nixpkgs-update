{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Applicative ((<**>), (<|>))
import Control.Exception
import Data.Maybe (isJust)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import DeleteMerged (deleteMerged)
import qualified Options.Applicative as Opt
import Shelly
import Update (updateAll)
import Utils (Options(..))

default (T.Text)

data Mode
    = Update
    | DeleteMerged

modeParser :: Opt.Parser Mode
modeParser =
    Opt.flag' Update (Opt.long "update" <> Opt.help "Update packages (default mode)" )
    <|> Opt.flag' DeleteMerged ( Opt.long "delete-merged" <> Opt.help "Delete branches that were already merged" )

programInfo :: Opt.ParserInfo Mode
programInfo = Opt.info (modeParser <**> Opt.helper)
    (Opt.fullDesc
    <> Opt.progDesc "Update packages in nixpkgs repository"
    <> Opt.header "nixpkgs-update" )

makeOptions :: Sh Options
makeOptions = do
    dryRun <- isJust <$> get_env "DRY_RUN"
    workingDir <- (</> ".nixpkgs-update") <$> get_env_text "HOME"
    githubToken <- T.strip <$> readfile "github_token.txt"
    return $ Options dryRun workingDir githubToken

setUpEnvironment :: Options -> Sh ()
setUpEnvironment options = do
    setenv "PAGER" ""
    setenv "GITHUB_TOKEN" (githubToken options)

main :: IO ()
main = shelly $ do
    mode <- liftIO $ Opt.execParser programInfo

    options <- makeOptions

    setUpEnvironment options

    case mode of
        DeleteMerged -> deleteMerged
        Update -> updateAll options
