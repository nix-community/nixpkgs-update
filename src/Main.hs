{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Control.Exception
import qualified Data.Text as T
import Shelly
import Prelude hiding (log)
import Utils (Options(..), Version, ExitCode(..), setupNixpkgs, parseUpdates, tRead, canFail)
import Data.Text (Text)
import Data.Maybe (isJust)
import Update (updatePackage)
import Data.Semigroup ((<>))
default (T.Text)


log' logFile msg = do
    runDate <- cmd "date" "-Iseconds"
    appendfile logFile (runDate <> msg)


makeOptions :: Sh Options
makeOptions = do
    dryRun <- isJust <$> get_env "DRY_RUN"
    workingDir <- (</> ".nix-update") <$> get_env_text "HOME"
    githubToken <- cmd "cat" "github_token.txt"
    return $ Options dryRun workingDir githubToken

setUpEnvironment :: Options -> Sh ()
setUpEnvironment options = do
    setenv "PAGER" ""
    setenv "GITHUB_TOKEN" (githubToken options)

main :: IO ()
main = shelly $ do
    options <- makeOptions

    let logFile = workingDir options </> "ups.log"

    mkdir_p (workingDir options)
    touchfile logFile

    setUpEnvironment options

    updates <- cmd "cat" "packages-to-update.txt"

    setupNixpkgs

    let log = log' logFile

    appendfile logFile "\n\n"
    log "New run of ups.sh"

    loop options log (parseUpdates updates) 0

loop :: Options -> (Text -> Sh ()) -> [(Text, Version, Version)] -> Int -> Sh ()
loop _ log [] _ = log "ups.sh finished"
loop options log ((package, oldVersion, newVersion) : moreUpdates) okToPrAt = do
    log package

    updated <- catch_sh
      (updatePackage options package oldVersion newVersion okToPrAt)
      (\ e ->
         case e of
           ExitCode 0 -> return True
           ExitCode _ -> return False)

    okToPrAt <-
        if updated then do
            log "SUCCESS"
            tRead <$> cmd "date" "+%s" "-d" "+15 minutes"
        else do
            log "FAIL"
            return okToPrAt

    loop options log moreUpdates okToPrAt

