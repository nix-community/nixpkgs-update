{-# LANGUAGE TemplateHaskell #-}

module Gpg
  ( binPath,
    recvKeys,
    verify,
  )
where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Language.Haskell.TH.Env (envQ)
import OurPrelude

binPath :: String
binPath = fromJust ($$(envQ "GPG") :: Maybe String) <> "/bin"

recvKeys ::
  MonadIO m =>
  Text ->
  ExceptT Text m Text
recvKeys releaseFingerprints =
  ourReadProcess_
    (proc (binPath <> "/gpg") (["--keyserver", "hkps://keyserver.ubuntu.com", "--recv-keys", T.unpack releaseFingerprints]))
    & fmapRT (fst >>> T.strip)

verify ::
  MonadIO m =>
  Text ->
  Text ->
  ExceptT Text m Text
verify sigFilePath filePath =
  ourReadProcess_
    (proc (binPath <> "/gpg") (["--verify", T.unpack sigFilePath, T.unpack filePath]))
    & fmapRT (fst >>> T.strip)


