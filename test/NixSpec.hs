{-# LANGUAGE OverloadedStrings #-}

module NixSpec where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Nix
import Test.Hspec
import qualified Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "unsupported hostPlatform failures" do
    it "detects Nix's metadata platform rejection" do
      let failure =
            T.unlines
              [ "error:",
                "       ... while evaluating the attribute 'drvPath'",
                "",
                "       error: Package 'tart-2.29.0' in /nix/store/source/pkgs/by-name/ta/tart/package.nix:123 is not available on the requested hostPlatform:",
                "         hostPlatform.config = \"x86_64-unknown-linux-gnu\";",
                "         package.meta.platforms = [ \"aarch64-darwin\" \"x86_64-darwin\" ];"
              ]

      Nix.isUnsupportedHostPlatformFailure failure `shouldBe` True

    it "does not match ordinary build failures" do
      let failure =
            T.unlines
              [ "nix build failed.",
                "error: builder for '/nix/store/example.drv' failed with exit code 2",
                "make: *** [Makefile:10: all] Error 2"
              ]

      Nix.isUnsupportedHostPlatformFailure failure `shouldBe` False

  describe "allowUnsupportedSystem build options" do
    it "preserves common policy while allowing unsupported systems" do
      let options = Utils.nixBuildOptionsAllowUnsupported
      let config = lookupArg "config" options

      options `shouldSatisfy` containsSequence ["--option", "sandbox", "true"]
      config `shouldSatisfy` maybe False (List.isInfixOf "allowUnfree = true")
      config `shouldSatisfy` maybe False (List.isInfixOf "allowAliases = false")
      config `shouldSatisfy` maybe False (List.isInfixOf "allowUnsupportedSystem = true")

containsSequence :: Eq a => [a] -> [a] -> Bool
containsSequence needle haystack =
  any (needle `List.isPrefixOf`) (List.tails haystack)

lookupArg :: String -> [String] -> Maybe String
lookupArg name ("--arg" : key : value : rest)
  | key == name = Just value
  | otherwise = lookupArg name (key : value : rest)
lookupArg name (_ : rest) = lookupArg name rest
lookupArg _ [] = Nothing
