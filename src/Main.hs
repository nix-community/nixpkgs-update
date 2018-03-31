{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import qualified Data.Text as T
import Shelly
import Prelude hiding (log)
import Utils (Options(..), Version, setupNixpkgs, parseUpdates, tRead)
import Data.Text (Text)
import Data.Maybe (isJust)
import Update (updatePackage)
import Data.Semigroup ((<>))
default (T.Text)
workingDir = "~/.nix-update"
logFile = workingDir </> "ups.log"

logSep = appendfile logFile "\n\n"

log msg = do
    runDate <- cmd "date" "-Iseconds"
    appendfile logFile (runDate <> msg)


makeOptions :: Sh Options
makeOptions = do
    dryRun <- isJust <$> get_env "DRY_RUN"
    return $ Options dryRun

main :: IO ()
main = shelly $ do
    options <- makeOptions

    mkdir_p workingDir
    touchfile logFile

    githubToken <- cmd "cat" "github_token.txt"

    updates <- cmd "cat" "packages-to-update.txt"

    setupNixpkgs

    logSep
    log "New run of ups.sh"

    loop options (parseUpdates updates) 0

loop :: Options -> [(Text, Version, Version)] -> Int -> Sh ()
loop _ [] _ = log "ups.sh finished"
loop options ((package, oldVersion, newVersion) : moreUpdates) okToPrAt = do
    log package

    updated <- updatePackage options package oldVersion newVersion okToPrAt

    okToPrAt <-
        if updated then do
            log "SUCCESS"
            tRead <$> cmd "date" "+%s" "-d" "+15 minutes"
        else do
            log "FAIL"
            return okToPrAt

    loop options moreUpdates okToPrAt

