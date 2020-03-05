module DoctestSpec where

import Test.DocTest
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Doctests" do
    it "should all pass" do
      doctest
        [ "-isrc",
          "-XOverloadedStrings",
          "-XDataKinds",
          "-XFlexibleContexts",
          "-XGADTs",
          "-XLambdaCase",
          "-XPolyKinds",
          "-XRankNTypes",
          "-XScopedTypeVariables",
          "-XTypeApplications",
          "-XTypeFamilies",
          "-XTypeOperators",
          "-XBlockArguments",
          "-flate-specialise",
          "-fspecialise-aggressively",
          "-fplugin=Polysemy.Plugin",
          "src/Version.hs",
          "src/GH.hs",
          "src/Time.hs"
        ]
