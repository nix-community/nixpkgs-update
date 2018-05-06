{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Outpaths
where

import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           NeatInterpolation (text)
import           Shelly
import           Utils
default (Text)

outPathsExpr = [text|

(let
  lib = import ./lib;
  hydraJobs = import ./pkgs/top-level/release.nix
    # Compromise: accuracy vs. resources needed for evaluation.
    {
      supportedSystems = [
        "aarch64-linux"
        "i686-linux"
        "x86_64-linux"
        "x86_64-darwin"
      ];

      nixpkgsArgs = {
        config = {
          allowBroken = true;
          allowUnfree = true;
          allowInsecurePredicate = x: true;
          checkMeta = true;

          handleEvalIssue = reason: errormsg:
            if reason == "unknown-meta"
              then abort errormsg
              else true;

          inHydra = true;
        };
      };
    };
  recurseIntoAttrs = attrs: attrs // { recurseForDerivations = true; };

  # hydraJobs leaves recurseForDerivations as empty attrmaps;
  # that would break nix-env and we also need to recurse everywhere.
  tweak = lib.mapAttrs
    (name: val:
      if name == "recurseForDerivations" then true
      else if lib.isAttrs val && val.type or null != "derivation"
              then recurseIntoAttrs (tweak val)
      else val
    );

  # Some of these contain explicit references to platform(s) we want to avoid;
  # some even (transitively) depend on ~/.nixpkgs/config.nix (!)
  blacklist = [
    "tarball" "metrics" "manual"
    "darwin-tested" "unstable" "stdenvBootstrapTools"
    "moduleSystem" "lib-tests" # these just confuse the output
  ];

in
  tweak (builtins.removeAttrs hydraJobs blacklist))
|]

--downloadOutPath :: Sh ()

outPath :: Sh Text
outPath = sub $ do
  Utils.setupNixpkgs
  cmd "curl" "-o" "outpaths.nix" "https://raw.githubusercontent.com/NixOS/ofborg/released/ofborg/src/outpaths.nix"
  setenv "GC_INITIAL_HEAP_SIZE" "4g"
  cmd "nix-env" "-f" "./outpaths.nix" "-qaP" "--no-name" "--out-path" "--arg" "checkMeta" "true"
