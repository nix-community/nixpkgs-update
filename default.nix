{ pkgs ? import (import ./nix/sources.nix).nixpkgs {config = { allowBroken = true; };},
returnShellEnv ? pkgs.lib.inNixShell
}:

let

  compiler = pkgs.haskell.packages.ghc883;

  inherit (pkgs.haskell.lib) dontCheck doJailbreak overrideCabal;

  pkg = compiler.developPackage {
    name = "nixpkgs-update";
    root = builtins.path { name = "nixpgks-update-src"; path = ./.; };
    overrides = self: super: { };
    source-overrides = { };
    inherit returnShellEnv;
  };

in pkg.overrideAttrs (attrs: {
  propagatedBuildInputs = with pkgs; [
    nix
    git
    getent
    gitAndTools.hub
    jq
    tree
    gist
  ];
})
