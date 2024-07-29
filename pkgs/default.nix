{ nixpkgs
, mmdoc
, system
, self
, ...
}:

let

  pkgs = import nixpkgs { inherit system; config = { allowBroken = true; }; };

  drvAttrs = attrs: with pkgs; {
    NIX = nix;
    GIT = git;
    JQ = jq;
    TREE = tree;
    GIST = gist;
    # TODO: are there more coreutils paths that need locking down?
    TIMEOUT = coreutils;
    NIXPKGSREVIEW = nixpkgs-review;
  };

  haskellPackages = pkgs.haskellPackages.override {
    overrides = _: haskellPackages: {
      polysemy-plugin = pkgs.haskell.lib.dontCheck haskellPackages.polysemy-plugin;
      polysemy = pkgs.haskell.lib.dontCheck haskellPackages.polysemy;
      http-api-data = pkgs.haskell.lib.doJailbreak haskellPackages.http-api-data;
      nixpkgs-update =
        pkgs.haskell.lib.justStaticExecutables (
          pkgs.haskell.lib.failOnAllWarnings (
            pkgs.haskell.lib.disableExecutableProfiling (
              pkgs.haskell.lib.disableLibraryProfiling (
                pkgs.haskellPackages.generateOptparseApplicativeCompletions [ "nixpkgs-update" ] (
                  (haskellPackages.callPackage ../nixpkgs-update.nix { }).overrideAttrs drvAttrs
                )
              )
            )
          )
        );
    };
  };

  shell = haskellPackages.shellFor {
    nativeBuildInputs = with pkgs; [
      cabal-install
      ghcid
      haskellPackages.cabal2nix
    ];
    packages = ps: [ ps.nixpkgs-update ];
    shellHook = ''
    '';
  };

  doc = pkgs.stdenvNoCC.mkDerivation rec {
    name = "nixpkgs-update-doc";
    src = self;
    phases = [ "mmdocPhase" ];
    mmdocPhase = "${mmdoc.packages.${system}.mmdoc}/bin/mmdoc nixpkgs-update $src/doc $out";
  };

in
{
  nixpkgs-update = haskellPackages.nixpkgs-update;
  default = haskellPackages.nixpkgs-update;
  nixpkgs-update-doc = doc;
  devShell = shell;
}
