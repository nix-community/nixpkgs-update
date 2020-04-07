{-# LANGUAGE OverloadedStrings #-}

module Rewrite
  ( Args (..),
    runAll,
    golangModuleVersion,
    quotedUrls,
    quotedUrlsET,
    rustCrateVersion,
    version,
  )
where

import qualified Data.Text as T
import qualified File
import qualified Nix
import OurPrelude
import qualified Polysemy.Error as Error
import Polysemy.Output (Output, output)
import qualified Process
import qualified Utils
  ( UpdateEnv (..),
    runLog,
  )
import Prelude hiding (log)

{-
 This module contains rewrite functions that make some modification to the
 nix derivation. These are in the IO monad so that they can do things like
 re-run nix-build to recompute hashes, but morally they should just stick to
 editing the derivationFile for their one stated purpose.

 The return contract is:
 - If it makes a modification, it should return a simple message to attach to
   the pull request description to provide context or justification for code
   reviewers (e.g., a GitHub issue or RFC).
 - If it makes no modification, return None
 - If it throws an exception, nixpkgs-update will be aborted for the package and
   no other rewrite functions will run.

  TODO: Setup some unit tests for these!
-}
data Args
  = Args
      { updateEnv :: Utils.UpdateEnv,
        attrPath :: Text,
        derivationFile :: FilePath,
        derivationContents :: Text
      }

runAll :: (Text -> IO ()) -> Args -> ExceptT Text IO [Text]
runAll log rwArgs = do
  msg1 <- Rewrite.version log rwArgs
  msg2 <- Rewrite.rustCrateVersion log rwArgs
  msg3 <- Rewrite.golangModuleVersion log rwArgs
  msg4 <- Rewrite.quotedUrlsET log rwArgs
  return $ catMaybes [msg1, msg2, msg3, msg4]

--------------------------------------------------------------------------------
-- The canonical updater: updates the src attribute and recomputes the sha256
version :: MonadIO m => (Text -> m ()) -> Args -> ExceptT Text m (Maybe Text)
version log args@(Args _ _ _ drvContents) = do
  lift $ log "[version]"
  if Nix.numberOfFetchers drvContents > 1 || Nix.numberOfHashes drvContents > 1
    then do
      lift $ log "[version] generic version rewriter does not support multiple hashes"
      return Nothing
    else do
      srcVersionFix args
      lift $ log "[version] updated version and sha256"
      return $ Just "Version update"

--------------------------------------------------------------------------------
-- Rewrite meta.homepage (and eventually other URLs) to be quoted if not
-- already, as per https://github.com/NixOS/rfcs/pull/45
quotedUrls ::
  Members '[Process.Process, File.File, Error Text, Output Text] r =>
  Args ->
  Sem r (Maybe Text)
quotedUrls (Args _ attrPth drvFile _) = do
  output "[quotedUrls]"
  homepage <- Nix.getHomepage attrPth
  -- Bit of a hack, but the homepage that comes out of nix-env is *always*
  -- quoted by the nix eval, so we drop the first and last characters.
  let stripped = T.init . T.tail $ homepage
  let goodHomepage = "homepage = " <> homepage <> ";"
  urlReplaced1 <- File.replace ("homepage = " <> stripped <> ";") goodHomepage drvFile
  urlReplaced2 <- File.replace ("homepage = " <> stripped <> " ;") goodHomepage drvFile
  urlReplaced3 <- File.replace ("homepage =" <> stripped <> ";") goodHomepage drvFile
  urlReplaced4 <- File.replace ("homepage =" <> stripped <> "; ") goodHomepage drvFile
  if urlReplaced1 || urlReplaced2 || urlReplaced3 || urlReplaced4
    then do
      output "[quotedUrls]: added quotes to meta.homepage"
      return $ Just "Quoted meta.homepage for [RFC 45](https://github.com/NixOS/rfcs/pull/45)"
    else do
      output "[quotedUrls] nothing found to replace"
      return Nothing

quotedUrlsET :: MonadIO m => (Text -> IO ()) -> Args -> ExceptT Text m (Maybe Text)
quotedUrlsET log rwArgs =
  ExceptT
    $ liftIO
      . runFinal
      . embedToFinal @IO
      . Error.runError
      . Process.runIO
      . File.runIO
      . Utils.runLog log
    $ quotedUrls rwArgs

--------------------------------------------------------------------------------
-- Rewrite Rust on rustPlatform.buildRustPackage
-- This is basically `version` above, but with a second pass to also update the
-- cargoSha256 vendor hash.
rustCrateVersion :: MonadIO m => (Text -> m ()) -> Args -> ExceptT Text m (Maybe Text)
rustCrateVersion log args@(Args _ attrPth drvFile drvContents) = do
  lift $ log "[rustCrateVersion]"
  if not (T.isInfixOf "cargoSha256" drvContents)
    then do
      lift $ log "[rustCrateVersion] No cargoSha256 found"
      return Nothing
    else do
      -- This starts the same way `version` does, minus the assert
      srcVersionFix args
      -- But then from there we need to do this a second time for the cargoSha256!
      oldCargoSha256 <- Nix.getAttr "cargoSha256" attrPth
      _ <- lift $ File.replaceIO oldCargoSha256 Nix.sha256Zero drvFile
      newCargoSha256 <- Nix.getHashFromBuild attrPth
      when (oldCargoSha256 == newCargoSha256) $ throwE "cargoSha256 hashes equal; no update necessary"
      lift . log $ "[rustCrateVersion] Replacing cargoSha256 with " <> newCargoSha256
      _ <- lift $ File.replaceIO Nix.sha256Zero newCargoSha256 drvFile
      -- Ensure the package actually builds and passes its tests
      Nix.build attrPth
      lift $ log "[rustCrateVersion] Finished updating Crate version and replacing hashes"
      return $ Just "Rust version update"

--------------------------------------------------------------------------------
-- Rewrite Golang packages with buildGoModule
-- This is basically `version` above, but with a second pass to also update the
-- modSha256 go vendor hash.
golangModuleVersion :: MonadIO m => (Text -> m ()) -> Args -> ExceptT Text m (Maybe Text)
golangModuleVersion log args@(Args _ attrPth drvFile drvContents) = do
  lift $ log "[golangModuleVersion]"
  if not (T.isInfixOf "buildGoModule" drvContents && T.isInfixOf "modSha256" drvContents)
    then do
      lift $ log "[golangModuleVersion] Not a buildGoModule package with modSha256"
      return Nothing
    else do
      -- This starts the same way `version` does, minus the assert
      srcVersionFix args
      -- But then from there we need to do this a second time for the modSha256!
      oldModSha256 <- Nix.getAttr "modSha256" attrPth
      lift . log $ "[golangModuleVersion] Found old modSha256 = " <> oldModSha256
      _ <- lift $ File.replaceIO oldModSha256 Nix.sha256Zero drvFile
      newModSha256 <- Nix.getHashFromBuild attrPth
      when (oldModSha256 == newModSha256) $ throwE "modSha256 hashes equal; no update necessary"
      lift . log $ "[golangModuleVersion] Replacing modSha256 with " <> newModSha256
      _ <- lift $ File.replaceIO Nix.sha256Zero newModSha256 drvFile
      -- Ensure the package actually builds and passes its tests
      Nix.build attrPth
      lift $ log "[golangModuleVersion] Finished updating modSha256"
      return $ Just "Golang update"

--------------------------------------------------------------------------------
-- Common helper functions and utilities
-- Helper to update version and src attributes, re-computing the sha256.
-- This is done by the generic version upgrader, but is also a sub-component of some of the others.
srcVersionFix :: MonadIO m => Args -> ExceptT Text m ()
srcVersionFix (Args env attrPth drvFile _) = do
  oldHash <- Nix.getOldHash attrPth
  _ <- lift $ File.replaceIO (Utils.oldVersion env) (Utils.newVersion env) drvFile
  _ <- lift $ File.replaceIO oldHash Nix.sha256Zero drvFile
  newHash <- Nix.getHashFromBuild attrPth
  when (oldHash == newHash) $ throwE "Hashes equal; no update necessary"
  _ <- lift $ File.replaceIO Nix.sha256Zero newHash drvFile
  return ()
