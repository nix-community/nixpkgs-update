{
  description = "update nixpkgs automatically";

  inputs.mmdoc.url = "github:ryantm/mmdoc";
  inputs.mmdoc.inputs.nixpkgs.follows = "nixpkgs";

  inputs.treefmt-nix.url = "github:numtide/treefmt-nix";
  inputs.treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

  inputs.runtimeDeps.url = "github:NixOS/nixpkgs/nixos-unstable-small";

  nixConfig.extra-substituters = "https://nix-community.cachix.org";
  nixConfig.extra-trusted-public-keys = "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";

  outputs = { self, nixpkgs, mmdoc, treefmt-nix, runtimeDeps } @ args:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      eachSystem = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      treefmtEval = eachSystem (pkgs: treefmt-nix.lib.evalModule pkgs {
        projectRootFile = ".git/config";
        programs.ormolu.enable = true;
      });

      buildAttrs = eachSystem (pkgs: {
        packages = import ./pkgs/default.nix (args // { inherit (pkgs) system; });
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.git
            pkgs.curl
            pkgs.treefmt
          ];
        };
        treefmt = treefmtEval.${pkgs.system}.config.build.check self;
      });
    in
    {
      checks = eachSystem (pkgs: {
        treefmt = buildAttrs.${pkgs.system}.treefmt;
      });

      packages = eachSystem (pkgs: buildAttrs.${pkgs.system}.packages);
      devShells = eachSystem (pkgs: { default = buildAttrs.${pkgs.system}.devShell; });

      formatter = eachSystem (pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);
    };
}
