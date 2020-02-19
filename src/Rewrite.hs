{-# LANGUAGE OverloadedStrings #-}

module Rewrite
  ( Args (..),
    version,
    quotedUrls,
    rustCrateVersion,
    rustCargoFetcher,
  )
where

import qualified Data.Text as T
import qualified File
import qualified Nix
import OurPrelude
import qualified Utils
  ( UpdateEnv (..),
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

--------------------------------------------------------------------------------
-- The canonical updater: updates the src attribute and recomputes the sha256
version :: MonadIO m => (Text -> m ()) -> Args -> ExceptT Text m (Maybe Text)
version log (Args env attrPth drvFile drvContents) = do
  lift $ log "[version] started"
  if Nix.numberOfFetchers drvContents > 1 || Nix.numberOfHashes drvContents > 1
  then do
    lift $ log "[version]: generic version rewriter does not support multiple hashes"
    return Nothing
  else do
    oldHash <- Nix.getOldHash attrPth
    oldSrcUrl <- Nix.getSrcUrl attrPth
    -- Change the actual version
    lift $ File.replace (Utils.oldVersion env) (Utils.newVersion env) drvFile
    newSrcUrl <- Nix.getSrcUrl attrPth
    when (oldSrcUrl == newSrcUrl) $ throwE "Source url did not change. "
    lift $ File.replace oldHash Nix.sha256Zero drvFile
    newHash <- Nix.getHashFromBuild attrPth
    tryAssert "Hashes equal; no update necessary" (oldHash /= newHash)
    lift $ File.replace Nix.sha256Zero newHash drvFile
    lift $ log "[version]: updated version and sha256"
    return $ Just "Version update"

--------------------------------------------------------------------------------
-- Rewrite meta.homepage (and eventually other URLs) to be quoted if not
-- already, as per https://github.com/NixOS/rfcs/pull/45
quotedUrls :: MonadIO m => (Text -> m ()) -> Args -> ExceptT Text m (Maybe Text)
quotedUrls log (Args _ attrPth drvFile drvContents) = do
  lift $ log "[quotedUrls] started"
  homepage <- Nix.getHomepage attrPth
  -- Bit of a hack, but the homepage that comes out of nix-env is *always*
  -- quoted by the nix eval, so we drop the first and last characters.
  let stripped = T.init . T.tail $ homepage
  if "\"\"" == homepage || (not $ T.isInfixOf stripped drvContents)
    then do
      lift $ log "[quotedUrls] there is no meta.homepage specified in the drv file"
      return Nothing
    else
      if T.isInfixOf homepage drvContents
        then do
          lift $ log "[quotedUrls] meta.homepage is already correctly quoted"
          return Nothing
        else do
          File.replace stripped homepage drvFile
          lift $ log "[quotedUrls]: added quotes to meta.homepage"
          return $ Just "Quoted meta.homepage for [RFC 45](https://github.com/NixOS/rfcs/pull/45)"

--------------------------------------------------------------------------------
-- One time migration for #79975
rustCargoFetcher :: MonadIO m => (Text -> m ()) -> Args -> ExceptT Text m (Maybe Text)
rustCargoFetcher log (Args _ _ drvFile drvContents) = do
  lift $ log "[rustCargoFetcher] started"
  if not (T.isInfixOf "Delete this on next update; see #79975" drvContents)
    then do
      lift $ log "[rustCargoFetcher] nothing to do"
      return Nothing
    else do
      _ <- readProcess $ proc "sed" ["-i", "/.*Delete this on next update; see #79975 for details/,/^$/d", drvFile]
      lift . log $ "[rustCargoFetcher] Ran sed on " <> T.pack drvFile
      -- N.B. The actual hash fix will come from the next rewriter!
      return $ Just "Updated cargo fetcher for #79975"

--------------------------------------------------------------------------------
-- Rewrite Rust on rustPlatform.buildRustPackage
-- This is basically rewriteVersion above, but we do a second pass for the cargoSha256 vendor hash.
rustCrateVersion :: MonadIO m => (Text -> m ()) -> Args -> ExceptT Text m (Maybe Text)
rustCrateVersion log (Args env attrPth drvFile drvContents) = do
  lift $ log "Processing: rewriteRustCrateVersion"
  if not (T.isInfixOf "cargoSha256" drvContents)
    then do
      lift $ log "No cargoSha256 found"
      return Nothing
    else do
      oldHash <- Nix.getOldHash attrPth
      -- This starts the same way rewriteVersion does, minus the assert
      lift $ File.replace (Utils.oldVersion env) (Utils.newVersion env) drvFile
      lift $ File.replace oldHash Nix.sha256Zero drvFile
      newHash <- Nix.getHashFromBuild attrPth
      lift $ File.replace Nix.sha256Zero newHash drvFile
      -- But then from there we need to do this a second time!
      oldCargoSha256 <- Nix.getDrvAttr "cargoSha256" attrPth
      lift $ File.replace oldCargoSha256 Nix.sha256Zero drvFile
      newCargoSha256 <- Nix.getHashFromBuild attrPth
      lift . log $ "Replacing cargoSha256 with " <> newCargoSha256
      lift $ File.replace Nix.sha256Zero newCargoSha256 drvFile
      Nix.build attrPth
      lift $ log "Finished updating Crate version and replacing hashes"
      return Nothing
