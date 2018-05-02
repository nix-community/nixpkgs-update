{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Check
  ( checkResult
  ) where

import Control.Applicative (many)
import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Prelude hiding (FilePath)
import Shelly
import qualified Text.Regex.Applicative as RE
import Text.Regex.Applicative (RE, (=~))
import Utils (Options(..), Version, canFail, succeded)

default (T.Text)

-- | Run a program with a provided argument and report whether
-- it exits successfully
-- Failure hints that the argument is not supported.
checkBinaryHelp :: (Text -> Sh ()) -> FilePath -> Text -> Sh ()
checkBinaryHelp addToReport program argument =
  (whenM (succeded (cmd "timeout" "-k" "2" "1" program argument)) $
    addToReport $
      "- ran ‘" <> toTextIgnore program <> " " <> argument <>
      "’ got 0 exit code"

-- | Construct regex: [^\.]*${version}\.*\s*
versionRegex :: Text -> RE Char ()
versionRegex version =
  (\_ _ _ _ -> ()) <$> many (RE.psym (/= '.')) <*> RE.string (T.unpack version) <*>
  many (RE.sym '.') <*>
  many (RE.psym isSpace)

-- | Run a program with provided argument and report whether the output
-- mentions the expected version
checkVersionType :: (Text -> Sh ()) -> Version -> FilePath -> Text -> Sh ()
checkVersionType addToReport expectedVersion program argument =
  catchany_sh
  (do
      stdout <- canFail $ cmd "timeout" "-k" "2" "1" program argument
      stderr <- lastStderr
      when
        (isJust $
         (T.unpack . T.unwords . T.lines $ stdout <> "\n" <> stderr) =~
         versionRegex expectedVersion) $
        addToReport $
        "- ran ‘" <> toTextIgnore program <> " " <> argument <>
        "’ and found version " <>
        expectedVersion)
  (\ _ -> return ())

-- | Run a program with various version or help flags and report
-- when they succeded
checkBinary :: (Text -> Sh ()) -> Version -> FilePath -> Sh ()
checkBinary addToReport expectedVersion program = do
  checkBinaryHelp addToReport program "-h"
  checkBinaryHelp addToReport program "--help"
  checkBinaryHelp addToReport program "help"
  checkVersionType addToReport expectedVersion program "-V"
  checkVersionType addToReport expectedVersion program "-v"
  checkVersionType addToReport expectedVersion program "--version"
  checkVersionType addToReport expectedVersion program "version"
  checkVersionType addToReport expectedVersion program "-h"
  checkVersionType addToReport expectedVersion program "--help"
  checkVersionType addToReport expectedVersion program "help"

checkResult :: Options -> FilePath -> Version -> Sh Text
checkResult options resultPath expectedVersion = do
  home <- get_env_text "HOME"
  let logFile = workingDir options </> "check-result-log.tmp"
  setenv "EDITOR" "echo"
  setenv "HOME" "/homeless-shelter"
  let addToReport input = appendfile logFile (input <> "\n")
  tempdir <- fromText . T.strip <$> cmd "mktemp" "-d"
  chdir tempdir $ do
    rm_f logFile
    let binaryDir = resultPath </> "bin"
    binExists <- test_d binaryDir
    binaries <-
      if binExists
        then findWhen test_f (resultPath </> "bin")
        else return []
    forM_ binaries $ \binary ->
      checkBinary addToReport expectedVersion binary
    unlessM (succeded $ cmd "test" "-s" logFile) $
      addToReport
        "- Warning: no binary found that responded to help or version flags. (This warning appears even if the package isn't expected to have binaries.)"
    canFail $ cmd "grep" "-r" expectedVersion resultPath
    whenM ((== 0) <$> lastExitCode) $
      addToReport $
        "- found " <> expectedVersion <> " with grep in " <>
        toTextIgnore resultPath
    whenM
      (null <$>
       findWhen
         (\path ->
            ((expectedVersion `T.isInfixOf` toTextIgnore path) &&) <$>
            test_f path)
         resultPath) $
      addToReport $
        "- found " <> expectedVersion <> " in filename of file in " <>
        toTextIgnore resultPath
    setenv "HOME" home
    gist <- cmd "tree" resultPath -|- cmd "gist"
    unless (T.null gist) $ addToReport $ "- directory tree listing: " <> gist
  canFail $ readfile logFile
