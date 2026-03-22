{-# LANGUAGE OverloadedStrings #-}

module FailureDbSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson (Result (..), Value (..), fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import FailureDb (recordPackageUpdateFailure)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Temp (emptySystemTempFile)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

restoreEnvVar :: String -> Maybe String -> IO ()
restoreEnvVar k Nothing = unsetEnv k
restoreEnvVar k (Just v) = setEnv k v

-- | Avoid touching a developer DATABASE_URL while exercising NDJSON logging.
withFailureLogOnly :: FilePath -> IO a -> IO a
withFailureLogOnly path act =
  bracket
    ( do
        oldDb <- lookupEnv "DATABASE_URL"
        oldLog <- lookupEnv "NIXPKGS_UPDATE_FAILURE_LOG"
        unsetEnv "DATABASE_URL"
        setEnv "NIXPKGS_UPDATE_FAILURE_LOG" path
        pure (oldDb, oldLog)
    )
    ( \(oldDb, oldLog) -> do
        unsetEnv "NIXPKGS_UPDATE_FAILURE_LOG"
        restoreEnvVar "DATABASE_URL" oldDb
        restoreEnvVar "NIXPKGS_UPDATE_FAILURE_LOG" oldLog
    )
    (const act)

spec :: Spec
spec = do
  describe "FailureDb NDJSON log" $ do
    it "appends one JSON object with telemetry keys" $ do
      path <- emptySystemTempFile "nixpkgs-update-failure.ndjson"
      withFailureLogOnly path $ do
        recordPackageUpdateFailure "hello.world" "nix build failed.\n" (Just 2) (Just 42)
        content <- BSL.readFile path
        case Aeson.decode content of
          Nothing -> expectationFailure "NDJSON line is not valid JSON"
          Just (Object o) -> do
            KM.lookup (K.fromString "attr_path") o `shouldBe` Just (String "hello.world")
            case KM.lookup (K.fromString "failure_kind") o of
              Just (String s) -> T.length s `shouldSatisfy` (> 0)
              _ -> expectationFailure "failure_kind missing or not a string"
            case KM.lookup (K.fromString "failure_message") o of
              Just (String s) -> s `shouldSatisfy` T.isInfixOf "nix build failed"
              _ -> expectationFailure "failure_message missing or not a string"
            case KM.lookup (K.fromString "maintainer_count") o of
              Just v -> fromJSON v `shouldBe` Success (2 :: Int)
              Nothing -> expectationFailure "maintainer_count missing"
            case KM.lookup (K.fromString "rebuild_path_count") o of
              Just v -> fromJSON v `shouldBe` Success (42 :: Int)
              Nothing -> expectationFailure "rebuild_path_count missing"
            case KM.lookup (K.fromString "recorded_at_utc") o of
              Just (String s) -> T.length s `shouldSatisfy` (> 10)
              _ -> expectationFailure "recorded_at_utc missing or not a string"
            length (T.lines (TE.decodeUtf8 $ BSL.toStrict content)) `shouldBe` 1
          Just _ -> expectationFailure "NDJSON line is not a JSON object"
