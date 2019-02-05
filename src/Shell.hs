{-# LANGUAGE OverloadedStrings #-}

module Shell
  ( shE
  , shRE
  , shellyET
  , succeeded
  , canFail
  , ourShell
  , ourSilentShell
  ) where

import OurPrelude

import qualified Data.Text as T
import Shelly.Lifted
import Utils

-- | Set environment variables needed by various programs
setUpEnvironment :: Options -> Sh ()
setUpEnvironment o = do
  setenv "PAGER" ""
  setenv "GITHUB_TOKEN" (githubToken o)

ourSilentShell :: Options -> Sh a -> IO a
ourSilentShell o s =
  shelly $
  silently $ do
    setUpEnvironment o
    s

ourShell :: MonadIO m => Options -> Sh a -> m a
ourShell o s =
  shelly $
  verbosely $ do
    setUpEnvironment o
    s

shE :: Sh a -> Sh (Either Text a)
shE s = do
  r <- canFail s
  status <- lastExitCode
  case status of
    0 -> return $ Right r
    c -> return $ Left ("Exit code: " <> T.pack (show c))

-- A shell cmd we are expecting to fail and want to look at the output
-- of it.
shRE :: MonadIO m => Sh a -> m (Either Text Text)
shRE s =
  shelly $ do
    _ <- canFail s
    stderr <- lastStderr
    status <- lastExitCode
    case status of
      0 -> return $ Left ""
      _ -> return $ Right stderr

shellyET :: MonadIO m => Sh a -> ExceptT Text m a
shellyET = shE >>> shelly >>> ExceptT

canFail :: Sh a -> Sh a
canFail = errExit False

succeeded :: Sh a -> Sh Bool
succeeded s = do
  _ <- canFail s
  status <- lastExitCode
  return (status == 0)
