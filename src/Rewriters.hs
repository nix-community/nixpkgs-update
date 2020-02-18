{-# LANGUAGE OverloadedStrings #-}

module Rewriters
  ( RewriteArgs (..),
    rewriteVersion,
    rewriteQuotedUrls,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified File
import qualified Nix
import OurPrelude
import qualified Utils
  ( UpdateEnv (..),
  )

-- This module contains rewrite functions that make some modification to the
-- nix derivation. These are in the IO monad so that they can do things like
-- re-run nix-build to recompute hashes, but morally they should just stick to
-- editing the derivationFile for their one stated purpose.
-- TODO: This is a work in progress. The rewriteQuotedUrls rewriter should be
-- really easy to unit test, so we should setup a rewriters test framework that
-- uses some mock.nix files and rewrites them, then asserts the expected diff.

data RewriteArgs
  = RewriteArgs
      { updateEnv :: Utils.UpdateEnv,
        attrPath :: Text,
        derivationFile :: FilePath
      }

--------------------------------------------------------------------------------
-- The canonical updater: updates the src attribute and recomputes the sha256
rewriteVersion :: MonadIO m => RewriteArgs -> ExceptT Text m ()
rewriteVersion (RewriteArgs env attrPth drvFile) = do
  oldHash <- Nix.getOldHash attrPth
  -- Change the actual version
  lift $ File.replace (Utils.oldVersion env) (Utils.newVersion env) drvFile
  lift $ File.replace oldHash Nix.sha256Zero drvFile
  newHash <- Nix.getHashFromBuild attrPth
  lift $ File.replace Nix.sha256Zero newHash drvFile

--------------------------------------------------------------------------------
-- Rewrite meta.homepage (and eventually other URLs) to be quoted if not
-- already, as per https://github.com/NixOS/rfcs/pull/45
rewriteQuotedUrls :: MonadIO m => RewriteArgs -> ExceptT Text m ()
rewriteQuotedUrls (RewriteArgs _ attrPth drvFile) = do
  homepage <- Nix.getHomepage attrPth
  contents <- liftIO $ T.readFile drvFile
  -- The homepage that comes out of nix-env is *always* quoted by the nix eval,
  -- so we drop the first and last characters. Bit of a hack but it works.
  let stripped = T.init . T.tail $ homepage
  unless (T.isInfixOf homepage contents) $
    File.replace stripped homepage drvFile
