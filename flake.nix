{
  description  "A flake for nixpkgs-update";

  inputs.flake-utils.url  "github:numtide/flake-utils";
  inputs.nixpkgs-review.url  "github:mic92/nixpkgs-review";
  inputs.flake-compat  { url  "github:edolstra/flake-compat"; flake = false; };
  inputs.nixpkgs  { type  "github"; owner = "nixos"; repo = "nixpkgs"; };

  outputs  { self, flake-utils, flake-compat, nixpkgs, nixpkgs-review }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs =  nixpkgs {  system; config = { allowBroken = true; }; };

        developPackageAttrs = {
          name  "nixpkgs-update";
          root  self;
          returnShellEnv  false;
        };

        drvAttrs  attrs: pkgs; {
          NIX  nix;
          GIT  git;
          HUB  gitAndTools.hub;
          JQ  jq;
          TREE  tree;
          GIST  gist;
          # TODO: are there more coreutils paths that need locking down?
          TIMEOUT  coreutils;
          NIXPKGSREVIEW  (import nixpkgs-review { inherit pkgs; });
        };

        haskellPackages  pkgs.haskell.packages.ghc8102.override {
          overrides  _: haskellPackages: {
            nixpkgs-update 
              pkgs.haskell.lib.justStaticExecutables (
                pkgs.haskell.lib.failOnAllWarnings (
                  pkgs.haskell.lib.disableExecutableProfiling (
                    pkgs.haskell.lib.disableLibraryProfiling (
                      pkgs.haskell.lib.generateOptparseApplicativeCompletion "nixpkgs-update" (
                        (haskellPackages.developPackage developPackageAttrs).overrideAttrs drvAttrs
                      )
                    )
                  )
                )
              );
          };
        };

        shell  haskellPackages.shellFor {
          nativeBuildInputs  pkgs; [
            cabal-install
            ghcid
          ];
          packages  ps: [ ps.nixpkgs-update ];
          shellHook  ''
          '';
        };

      in
      {
        devShell  shell;
        packages.nixpkgs-update  haskellPackages.nixpkgs-update;
        defaultPackage  haskellPackages.nixpkgs-update;
      });
}
