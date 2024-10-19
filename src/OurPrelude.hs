{-# LANGUAGE PartialTypeSignatures #-}

module OurPrelude
  ( (>>>),
    (<|>),
    (<>),
    (</>),
    (<&>),
    (&),
    module Control.Error,
    module Control.Monad.Except,
    module Control.Monad.Trans.Class,
    module Control.Monad.IO.Class,
    module Data.Bifunctor,
    module System.Process.Typed,
    module Polysemy,
    module Polysemy.Error,
    ignoreExitCodeException,
    Set,
    Text,
    Vector,
    interpolate,
    tshow,
    tryIOTextET,
    whenM,
    ourReadProcess_,
    ourReadProcess_Sem,
    ourReadProcessInterleaved_,
    ourReadProcessInterleavedBS_,
    ourReadProcessInterleaved,
    ourReadProcessInterleavedSem,
    silently,
    bytestringToText,
  )
where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Error
import qualified Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Set (Set)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Vector (Vector)
import Language.Haskell.TH.Quote
import qualified NeatInterpolation
import Polysemy
import Polysemy.Error hiding (note, try, tryJust)
import qualified Process as P
import System.Exit
import System.FilePath ((</>))
import System.Process.Typed

interpolate :: QuasiQuoter
interpolate = NeatInterpolation.text

tshow :: (Show a) => a -> Text
tshow = show >>> pack

tryIOTextET :: (MonadIO m) => IO a -> ExceptT Text m a
tryIOTextET = syncIO >>> fmapLT tshow

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM c a = c >>= \res -> when res a

bytestringToText :: BSL.ByteString -> Text
bytestringToText = BSL.toStrict >>> (T.decodeUtf8With T.lenientDecode)

ourReadProcessInterleavedBS_ ::
  (MonadIO m) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  ExceptT Text m BSL.ByteString
ourReadProcessInterleavedBS_ = readProcessInterleaved_ >>> tryIOTextET

ourReadProcess_ ::
  (MonadIO m) =>
  ProcessConfig stdin stdout stderr ->
  ExceptT Text m (Text, Text)
ourReadProcess_ = readProcess_ >>> tryIOTextET >>> fmapRT (\(stdout, stderr) -> (bytestringToText stdout, bytestringToText stderr))

ourReadProcess_Sem ::
  (Members '[P.Process] r) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Sem r (Text, Text)
ourReadProcess_Sem =
  P.read_ >>> fmap (\(stdout, stderr) -> (bytestringToText stdout, bytestringToText stderr))

ourReadProcessInterleaved_ ::
  (MonadIO m) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  ExceptT Text m Text
ourReadProcessInterleaved_ =
  readProcessInterleaved_ >>> tryIOTextET >>> fmapRT bytestringToText

ourReadProcessInterleaved ::
  (MonadIO m) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  ExceptT Text m (ExitCode, Text)
ourReadProcessInterleaved =
  readProcessInterleaved
    >>> tryIOTextET
    >>> fmapRT (\(a, b) -> (a, bytestringToText b))

ourReadProcessInterleavedSem ::
  (Members '[P.Process] r) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Sem r (ExitCode, Text)
ourReadProcessInterleavedSem =
  P.readInterleaved
    >>> fmap (\(a, b) -> (a, bytestringToText b))

silently :: ProcessConfig stdin stdout stderr -> ProcessConfig () () ()
silently = setStderr closed >>> setStdin closed >>> setStdout closed

ignoreExitCodeException :: IO () -> IO ()
ignoreExitCodeException a = Control.Exception.catch a (\(_e :: ExitCodeException) -> pure ())
