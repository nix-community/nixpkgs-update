{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Fix
import Data.Map.Strict
import Nix.Eval
import Nix.Atoms
import Nix.Parser

main :: IO ()
main = ourEval
