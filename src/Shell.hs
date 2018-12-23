{-# LANGUAGE OverloadedStrings #-}

module Shell
  ( shE
  , shRE
  , shellyET
  , succeded
  , canFail
  , orElse
  , ourShell
  , ourSilentShell
  ) where

import Control.Category ((>>>))
import Control.Error
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Shelly.Lifted
import Utils

-- | Set environment variables needed by various programs
setUpEnvironment :: Options -> Sh ()
setUpEnvironment options = do
  setenv "PAGER" ""
  setenv "GITHUB_TOKEN" (githubToken options)

ourSilentShell :: Options -> Sh a -> IO a
ourSilentShell o s =
  shelly $
  silently $ do
    setUpEnvironment o
    s

ourShell :: Options -> Sh a -> IO a
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
shRE :: Sh a -> Sh (Either Text Text)
shRE s = do
  canFail s
  stderr <- lastStderr
  status <- lastExitCode
  case status of
    0 -> return $ Left ""
    c -> return $ Right stderr

shellyET :: MonadIO m => Sh a -> ExceptT Text m a
shellyET = shE >>> shelly >>> ExceptT

canFail :: Sh a -> Sh a
canFail = errExit False

succeded :: Sh a -> Sh Bool
succeded s = do
  canFail s
  status <- lastExitCode
  return (status == 0)

orElse :: Sh a -> Sh a -> Sh a
orElse a b = do
  v <- canFail a
  status <- lastExitCode
  if status == 0
    then return v
    else b

infixl 3 `orElse`
