--{-# LANGUAGE TemplateHaskell #-}

module Main where

import Nix.Parser
import Nix.Pretty

--import Control.Lens

main :: IO ()
main = do
  helloNixFileContents <- readFile "./hello.nix"
  print (fmap prettyNix (parseNixString helloNixFileContents))
