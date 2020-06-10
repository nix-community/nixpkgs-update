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

        pkg = (compiler.developPackage {
          name = "nixpkgs-update";
          root = self;
          overrides = self: super: { };
          source-overrides = { };
        }).overrideAttrs (attrs: {
          propagatedBuildInputs = with pkgs; [
            nix
            git
            getent
            gitAndTools.hub
            jq
            tree
            gist
            (import nixpkgs-review { inherit pkgs; })
            cabal-install # just for develpoment
          ];
        });
    in
    {
      packages.x86_64-linux.nixpkgs-update = pkg;
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.nixpkgs-update;
    };
}
