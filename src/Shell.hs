{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Control.Exception (SomeException)
import Control.Exception.Lifted (handle)
import qualified Data.Text as T
import Shelly.Lifted
import Utils

ourSilentShell :: Options -> Sh a -> IO a
ourSilentShell _ s = shelly $ Shelly.Lifted.silently s

ourShell :: MonadIO m => Options -> Sh a -> m a
ourShell _ s = shelly $ verbosely s

shE :: Sh a -> Sh (Either Text a)
shE s =
  handle
    (\(e :: SomeException) ->
       return $ Left ("Shell exception: " <> T.pack (show e))) $ do
    r <- canFail s
    status <- lastExitCode
    case status of
      0 -> return $ Right r
      c -> return $ Left ("Exit code: " <> T.pack (show c))

-- A shell cmd we are expecting to fail to look at stderr
shRE :: MonadIO m => Sh a -> m (Either Text Text)
shRE s =
  shelly $
  handle
    (\(e :: SomeException) ->
       return $ Left ("Shell exception: " <> T.pack (show e))) $ do
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
