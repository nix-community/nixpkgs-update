{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Rewrite
  ( Args (..),
    runAll,
    golangModuleVersion,
    quotedUrls,
    rustCrateVersion,
    version,
    redirectedUrls,
  )
where

import qualified Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Encoding.Error as T
import Data.Text.IO as T
import qualified File
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import qualified Nix
import OurPrelude
import System.Exit()
import Utils (UpdateEnv (..))
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
data Args = Args
  { updateEnv :: Utils.UpdateEnv,
    attrPath :: Text,
    derivationFile :: FilePath,
    derivationContents :: Text,
    hasUpdateScript :: Bool
  }

type Rewriter = (Text -> IO ()) -> Args -> ExceptT Text IO (Maybe Text)

type Plan = [(Text, Rewriter)]

plan :: Plan
plan =
  [ ("version", version),
    ("rustCrateVersion", rustCrateVersion),
    ("golangModuleVersion", golangModuleVersion),
    ("updateScript", updateScript),
    ("", quotedUrls) -- Don't change the logger
    --("redirectedUrl", Rewrite.redirectedUrls)
  ]

runAll :: (Text -> IO ()) -> Args -> ExceptT Text IO [Text]
runAll log args = do
  msgs <- forM plan $ \(name, f) -> do
    let log' msg =
          if T.null name
            then log msg
            else log $ ("[" <> name <> "] ") <> msg
    lift $ log' "" -- Print initial empty message to signal start of rewriter
    f log' args
  return $ catMaybes msgs

--------------------------------------------------------------------------------
-- The canonical updater: updates the src attribute and recomputes the sha256
version :: MonadIO m => (Text -> m ()) -> Args -> ExceptT Text m (Maybe Text)
version log args@Args {..} = do
  if
      | Nix.numberOfFetchers derivationContents > 1 || Nix.numberOfHashes derivationContents > 1 -> do
        lift $ log "generic version rewriter does not support multiple hashes"
        return Nothing
      | hasUpdateScript -> do
        lift $ log "skipping because derivation has updateScript"
        return Nothing
      | otherwise -> do
        srcVersionFix args
        lift $ log "updated version and sha256"
        return $ Just "Version update"

--------------------------------------------------------------------------------
-- Rewrite meta.homepage (and eventually other URLs) to be quoted if not
-- already, as per https://github.com/NixOS/rfcs/pull/45
quotedUrls :: (Text -> IO ()) -> Args -> ExceptT Text IO (Maybe Text)
quotedUrls log Args {..} = do
  lift $ log "[quotedUrls]"
  homepage <- Nix.getHomepage attrPath
  let goodHomepage = "homepage = " <> homepage <> ";"
  let replacer = \target -> File.replaceIO target goodHomepage derivationFile
  urlReplaced1 <- replacer ("homepage = " <> homepage <> ";")
  urlReplaced2 <- replacer ("homepage = " <> homepage <> " ;")
  urlReplaced3 <- replacer ("homepage =" <> homepage <> ";")
  urlReplaced4 <- replacer ("homepage =" <> homepage <> "; ")
  if urlReplaced1 || urlReplaced2 || urlReplaced3 || urlReplaced4
    then do
      lift $ log "[quotedUrls]: added quotes to meta.homepage"
      return $ Just "Quoted meta.homepage for [RFC 45](https://github.com/NixOS/rfcs/pull/45)"
    else do
      lift $ log "[quotedUrls] nothing found to replace"
      return Nothing

--------------------------------------------------------------------------------
-- Redirect homepage when moved.
redirectedUrls :: MonadIO m => (Text -> m ()) -> Args -> ExceptT Text m (Maybe Text)
redirectedUrls log Args {..} = do
  homepage <- Nix.getHomepage attrPath
  response <- liftIO $ do
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    request <- HTTP.parseRequest (T.unpack homepage)
    HTTP.httpLbs request manager
  let status = statusCode $ HTTP.responseStatus response
  if status `elem` [301, 308]
    then do
      lift $ log "Redirecting URL"
      let headers = HTTP.responseHeaders response
          location = lookup "Location" headers
      case location of
        Nothing -> do
          lift $ log "Server did not return a location"
          return Nothing
        Just ((T.decodeUtf8With T.lenientDecode) -> newHomepage) -> do
          _ <- File.replaceIO homepage newHomepage derivationFile
          lift $ log "Replaced homepage"
          return $
            Just $
              "Replaced homepage by "
                <> newHomepage
                <> " due http "
                <> (T.pack . show) status
    else do
      lift $ log "URL not redirected"
      return Nothing

--------------------------------------------------------------------------------
-- Rewrite Rust on rustPlatform.buildRustPackage
-- This is basically `version` above, but with a second pass to also update the
-- cargoSha256 vendor hash.
rustCrateVersion :: MonadIO m => (Text -> m ()) -> Args -> ExceptT Text m (Maybe Text)
rustCrateVersion log args@Args {..} = do
  if
      | and [(not (T.isInfixOf "cargoSha256" derivationContents)),(not (T.isInfixOf "cargoHash" derivationContents))] -> do
        lift $ log "No cargoSha256 or cargoHash found"
        return Nothing
      | hasUpdateScript -> do
        lift $ log "skipping because derivation has updateScript"
        return Nothing
      | otherwise -> do
        _ <- lift $ File.replaceIO "cargoSha256 =" "cargoHash =" derivationFile
        -- This starts the same way `version` does, minus the assert
        srcVersionFix args
        -- But then from there we need to do this a second time for the cargoHash!
        oldCargoHash <- Nix.getAttrString "cargoHash" attrPath
        _ <- lift $ File.replaceIO oldCargoHash Nix.fakeHash derivationFile
        newCargoHash <- Nix.getHashFromBuild attrPath
        when (oldCargoHash == newCargoHash) $ throwE ("cargo hashes equal; no update necessary: " <> oldCargoHash)
        lift . log $ "Replacing cargoHash with " <> newCargoHash
        _ <- lift $ File.replaceIO Nix.fakeHash newCargoHash derivationFile
        -- Ensure the package actually builds and passes its tests
        Nix.build attrPath
        lift $ log "Finished updating Crate version and replacing hashes"
        return $ Just "Rust version update"

--------------------------------------------------------------------------------
-- Rewrite Golang packages with buildGoModule
-- This is basically `version` above, but with a second pass to also update the
-- vendorSha256 go vendor hash.
golangModuleVersion :: MonadIO m => (Text -> m ()) -> Args -> ExceptT Text m (Maybe Text)
golangModuleVersion log args@Args {..} = do
  if
      | and [not (T.isInfixOf "buildGoModule" derivationContents && T.isInfixOf "vendorSha256" derivationContents), not (T.isInfixOf "buildGoModule" derivationContents && T.isInfixOf "vendorHash" derivationContents)] -> do
        lift $ log "Not a buildGoModule package with vendorSha256 or vendorHash"
        return Nothing
      | hasUpdateScript -> do
        lift $ log "skipping because derivation has updateScript"
        return Nothing
      | otherwise -> do
        _ <- lift $ File.replaceIO "vendorSha256 =" "vendorHash =" derivationFile
        -- This starts the same way `version` does, minus the assert
        srcVersionFix args
        -- But then from there we need to do this a second time for the vendorSha256!
        -- Note that explicit `null` cannot be coerced to a string by nix eval --raw
        oldVendorHash <- Nix.getAttrString "vendorHash" attrPath
        lift . log $ "Found old vendorHash = " <> oldVendorHash
        original <- liftIO $ T.readFile derivationFile
        _ <- lift $ File.replaceIO ("\"" <> oldVendorHash <> "\"") "null" derivationFile
        ok <- runExceptT $ Nix.build attrPath
        _ <-
          if isLeft ok
            then do
              _ <- liftIO $ T.writeFile derivationFile original
              _ <- lift $ File.replaceIO oldVendorHash Nix.fakeHash derivationFile
              newVendorHash <- Nix.getHashFromBuild attrPath
              _ <- lift $ File.replaceIO Nix.fakeHash newVendorHash derivationFile
              -- Note that on some small bumps, this may not actually change if go.sum did not
              lift . log $ "Replaced vendorHash with " <> newVendorHash
            else do
              lift . log $ "Set vendorHash to null"
        -- Ensure the package actually builds and passes its tests
        Nix.build attrPath
        lift $ log "Finished updating vendorHash"
        return $ Just "Golang update"

--------------------------------------------------------------------------------
-- Calls passthru.updateScript
updateScript :: MonadIO m => (Text -> m ()) -> Args -> ExceptT Text m (Maybe Text)
updateScript log Args {..} = do
  if hasUpdateScript
    then do
      (exitCode, msg) <- Nix.runUpdateScript attrPath
      case exitCode of
        ExitSuccess -> do
          lift $ log "Success"
          lift $ log msg
          return $ Just "Ran passthru.UpdateScript"
        ExitFailure num -> do
          throwE $ "[updateScript] Failed with exit code " <> tshow num <> "\n" <> msg
    else do
      lift $ log "skipping because derivation has no updateScript"
      return Nothing
--------------------------------------------------------------------------------
-- Common helper functions and utilities
-- Helper to update version and src attributes, re-computing the sha256.
-- This is done by the generic version upgrader, but is also a sub-component of some of the others.
srcVersionFix :: MonadIO m => Args -> ExceptT Text m ()
srcVersionFix Args {..} = do
  let UpdateEnv {..} = updateEnv
  oldHash <- Nix.getHash attrPath
  _ <- lift $ File.replaceIO oldVersion newVersion derivationFile
  _ <- lift $ File.replaceIO oldHash Nix.fakeHash derivationFile
  newHash <- Nix.getHashFromBuild attrPath
  when (oldHash == newHash) $ throwE "Hashes equal; no update necessary"
  _ <- lift $ File.replaceIO Nix.fakeHash newHash derivationFile
  return ()
