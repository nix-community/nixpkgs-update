{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Control.Applicative ((<|>), (<**>))
import Control.Exception
import qualified Data.Text as T
import Shelly
import Utils (Options(..))
import Data.Text (Text)
import Data.Maybe (isJust)
import DeleteMerged (deleteMerged)
import Update (updateAll)
import Data.Semigroup ((<>))
import qualified Options.Applicative as Opt

default (T.Text)


data Mode =
    Update
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
    githubToken <- cmd "cat" "github_token.txt"
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
