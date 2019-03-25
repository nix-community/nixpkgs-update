{-# LANGUAGE PartialTypeSignatures #-}

module OurPrelude
  ( (>>>)
  , (<|>)
  , (<>)
  , (&)
  , module Control.Error
  , module Control.Monad.Except
  , module Control.Monad.Trans.Class
  , module Control.Monad.IO.Class
  , module Data.Bifunctor
  , Set
  , Text
  , Vector
  , interpolate
  , tshow
  , tryIOTextET
  , whenM
  , ourReadProcessInterleaved_
  , runProcess
  ) where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Error
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as T
import Data.Vector (Vector)
import Language.Haskell.TH.Quote
import qualified NeatInterpolation
import System.Process.Typed

interpolate :: QuasiQuoter
interpolate = NeatInterpolation.text

tshow :: Show a => a -> Text
tshow = show >>> pack

tryIOTextET :: MonadIO m => IO a -> ExceptT Text m a
tryIOTextET = tryIO >>> fmapLT tshow

whenM :: Monad m => m Bool -> m () -> m ()
whenM c a = c >>= \res -> when res a

ourReadProcessInterleaved_ ::
     MonadIO m
  => ProcessConfig stdin stdoutIgnored stderrIgnored
  -> ExceptT Text m Text
ourReadProcessInterleaved_ processConfig =
  readProcessInterleaved_ processConfig & tryIOTextET &
  fmapRT (BSL.toStrict >>> T.decodeUtf8)
