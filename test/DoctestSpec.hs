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
    --          "-fplugin=Polysemy.Plugin",
    -- src/Process.hs:1:1: error:
    --  Can't find interface-file declaration for type constructor or class Polysemy.Internal.Union.LocateEffect
    --    Probable cause: bug in .hi-boot file, or inconsistent .hi file
    --    Use -ddump-if-trace to get an idea of which file caused the error

          "src/Version.hs",
          "src/GH.hs",
          "src/Time.hs"
        ]
