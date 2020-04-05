{ pkgs ? import (import ./nix/sources.nix).nixpkgs {config = { allowBroken = true; };},
returnShellEnv ? pkgs.lib.inNixShell
}:

let

  gitignore = import (import ./nix/sources.nix).gitignore { inherit (pkgs) lib; };
  inherit (gitignore) gitignoreSource;

  compiler = pkgs.haskell.packages.ghc883;

  inherit (pkgs.haskell.lib) dontCheck doJailbreak overrideCabal;

  root = gitignoreSource ./.;

  pkg = compiler.developPackage {
    name = "nixpkgs-update";
    overrides = self: super: { };
    source-overrides = { };
    inherit root returnShellEnv;
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
