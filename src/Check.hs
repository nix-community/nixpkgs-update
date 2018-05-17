{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Check
  ( checkResult
  ) where

import Control.Applicative (many)
import Control.Monad (forM)
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Prelude hiding (FilePath)
import Shelly
import qualified Text.Regex.Applicative as RE
import Text.Regex.Applicative (RE, (=~))
import Utils (Options(..), UpdateEnv (..), Version, canFail, succeded)

default (T.Text)

data BinaryCheck = BinaryCheck
  { filePath :: FilePath
  , zeroExitCode :: Bool
  , versionPresent :: Bool
  }

-- | Construct regex: [^\.]*${version}\.*\s*
versionRegex :: Text -> RE Char ()
versionRegex version =
  (\_ _ _ _ -> ()) <$> many (RE.psym (/= '.')) <*> RE.string (T.unpack version) <*>
  many (RE.sym '.') <*>
  many (RE.psym isSpace)

-- | Run a program with provided argument and report whether the output
-- mentions the expected version
checkBinary :: Text -> Version -> FilePath -> Sh BinaryCheck
checkBinary argument expectedVersion program =
  catchany_sh
  (do
      stdout <- canFail $ cmd "timeout" "-k" "2" "1" program argument
      code <- lastExitCode
      stderr <- lastStderr
      let hasVersion =
            isJust $ (T.unpack . T.unwords . T.lines $ stdout <> "\n" <> stderr) =~
            versionRegex expectedVersion
      return $ BinaryCheck program (code == 0) hasVersion)
  (\ _ -> return $ BinaryCheck program False False)

checks :: [Version -> FilePath -> Sh BinaryCheck]
checks =
  [ checkBinary ""
  , checkBinary "-V"
  , checkBinary "-v"
  , checkBinary "--version"
  , checkBinary "version"
  , checkBinary "-h"
  , checkBinary "--help"
  , checkBinary "help"
  ]

someChecks :: BinaryCheck -> [Sh BinaryCheck] -> Sh BinaryCheck
someChecks best [] = return best
someChecks best (c:rest) = do
  current <- c
  let nb = newBest current
  case nb of
    BinaryCheck _ True True -> return nb
    _ -> someChecks nb rest
  where
    newBest :: BinaryCheck -> BinaryCheck
    newBest (BinaryCheck _ currentExit currentVersionPresent) =
      BinaryCheck
      (filePath best)
      (zeroExitCode best || currentExit)
      (versionPresent best || currentVersionPresent)

-- | Run a program with various version or help flags and report
-- when they succeded
runChecks :: Version -> FilePath -> Sh BinaryCheck
runChecks expectedVersion program =
  someChecks (BinaryCheck program False False) checks'
  where
    checks' = map (\c -> c expectedVersion program) checks

checkReport :: BinaryCheck -> Text
checkReport (BinaryCheck p False False) = "- Warning: no invocation of " <> toTextIgnore p <> " had a zero exit code or showed the expected version"
checkReport (BinaryCheck p _ _) = "- " <> toTextIgnore p <> " passed the binary check."

successfullCheck :: BinaryCheck -> Bool
successfullCheck (BinaryCheck _ False False) = False
successfullCheck _ = True

checkResult :: UpdateEnv -> FilePath  -> Sh Text
checkResult updateEnv resultPath = do
  let expectedVersion = newVersion updateEnv
  home <- get_env_text "HOME"
  let logFile = workingDir (options updateEnv) </> "check-result-log.tmp"
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
    checks <- forM binaries $ \binary -> runChecks expectedVersion binary
    addToReport (T.intercalate "\n" (map checkReport checks))
    let passedZeroExitCode = (T.pack . show)
          (foldl
           (\sum c -> if zeroExitCode c then sum + 1 else sum) 0 checks :: Int)
        passedVersionPresent = (T.pack . show)
          (foldl
           (\sum c -> if versionPresent c then sum + 1 else sum) 0 checks :: Int)
        numBinaries = (T.pack . show) (length binaries)

    addToReport ("- " <> passedZeroExitCode <> " of " <> numBinaries <> " passed binary check by having a zero exit code.")
    addToReport ("- " <> passedVersionPresent <> " of " <> numBinaries <> " passed binary check by having the new version present in output.")
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
    gist1 <- cmd "tree" resultPath -|- cmd "gist"
    unless (T.null gist1) $ addToReport $ "- directory tree listing: " <> T.strip gist1
    gist2 <- cmd "du" "-h" resultPath -|- cmd "gist"
    unless (T.null gist2) $ addToReport $ "- du listing: " <> T.strip gist2
  canFail $ readfile logFile
