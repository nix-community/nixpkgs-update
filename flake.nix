{
  description = "A flake for nixpkgs-update";

  inputs.nixpkgs = {
    type = "github";
    owner = "nixos";
    repo = "nixpkgs";
    rev = "78bfdbb291fd20df0f0f65061ee3081610b0a48f";
  };

  inputs.nixpkgs-review = {
    type = "github";
    owner = "Mic92";
    repo = "nixpkgs-review";
    flake = false;
  };

  outputs = { self, nixpkgs, nixpkgs-review }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        config = { allowBroken = true; };
      };

      compiler = pkgs.haskell.packages.ghc883;

      developPackageAttrs = {
        name = "nixpkgs-update";
        root = self;
        overrides = self: super: { };
        source-overrides = { };
        modifier = pkgs.haskell.lib.disableLibraryProfiling;
        returnShellEnv = false;
      };

      developPackageShellAttrs = developPackageAttrs // {
        returnShellEnv = true;
      };

      drvAttrs = attrs: with pkgs; {
        # TODO: lock down coreutils paths too
        NIX = nix;
        GIT = git;
        HUB = gitAndTools.hub;
        JQ = jq;
        TREE = tree;
        GIST = gist;
        NIXPKGSREVIEW = (import nixpkgs-review { inherit pkgs; });
      };

      drvShellAttrs = attrs: (drvAttrs attrs) // (with pkgs; {
        nativeBuildInputs = attrs.nativeBuildInputs ++ [ cabal-install ghcid ];
      });

      pkg = (compiler.developPackage developPackageAttrs).overrideAttrs drvAttrs;

      shell = (compiler.developPackage developPackageShellAttrs).overrideAttrs drvShellAttrs;

    in {
      devShell = shell;
      packages.x86_64-linux.nixpkgs-update = pkg;
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.nixpkgs-update;
    };
}
