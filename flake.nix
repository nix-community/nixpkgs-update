{
  description = "update nixpkgs automatically";

  inputs.mmdoc.url = "github:ryantm/mmdoc";
  inputs.mmdoc.inputs.nixpkgs.follows = "nixpkgs";

  inputs.treefmt-nix.url = "github:numtide/treefmt-nix";
  inputs.treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

  nixConfig.extra-substituters = "https://nix-community.cachix.org";
  nixConfig.extra-trusted-public-keys = "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";

  outputs = { self, nixpkgs, mmdoc, treefmt-nix } @ args:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      eachSystem = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
      treefmtEval = eachSystem (pkgs: treefmt-nix.lib.evalModule pkgs {
        projectRootFile = ".git/config";
        programs.ormolu.enable = true;
      });
    in
    {
      checks.x86_64-linux =
        let
          packages = nixpkgs.lib.mapAttrs' (n: nixpkgs.lib.nameValuePair "package-${n}") self.packages.x86_64-linux;
          devShells = nixpkgs.lib.mapAttrs' (n: nixpkgs.lib.nameValuePair "devShell-${n}") self.devShells.x86_64-linux;
        in
        packages // devShells // {
          treefmt = treefmtEval.x86_64-linux.config.build.check self;
        };

      formatter = eachSystem (pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);

      packages.x86_64-linux = import ./pkgs/default.nix (args // { system = "x86_64-linux"; });
      devShells.x86_64-linux.default = self.packages."x86_64-linux".devShell;

      # nix flake check is broken for these when run on x86_64-linux
      # packages.x86_64-darwin = import ./pkgs/default.nix (args // { system = "x86_64-darwin"; });
      # devShells.x86_64-darwin.default = self.packages."x86_64-darwin".devShell;
    };
}
