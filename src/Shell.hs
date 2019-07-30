{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shell
  ( succeeded
  , canFail
  , ourShell
  ) where

import OurPrelude

import Shelly.Lifted
import Utils

ourShell :: MonadIO m => Options -> Sh a -> m a
ourShell _ s = shelly $ verbosely s

canFail :: Sh a -> Sh a
canFail = errExit False

succeeded :: Sh a -> Sh Bool
succeeded s = do
  _ <- canFail s
  status <- lastExitCode
  return (status == 0)
