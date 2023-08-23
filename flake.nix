{
  description = "update nixpkgs automatically";

  inputs.mmdoc.url = "github:ryantm/mmdoc";
  inputs.mmdoc.inputs.nixpkgs.follows = "nixpkgs";

  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

  nixConfig.extra-substituters = "https://nixpkgs-update.cachix.org";
  nixConfig.extra-trusted-public-keys = "nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8=";

  outputs = { self, nixpkgs, flake-parts, mmdoc } @ inputs:

    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem = {pkgs, system, ...}: {
        packages = import ./pkgs/default.nix { inherit system mmdoc nixpkgs; };
        # devShell.default = packages.${system}.devShell;
      };

    };
}
# {
#   # packages.x86_64-linux = import ./pkgs/default.nix (args // { system = "x86_64-linux"; });
#   # devShells.x86_64-linux.default = self.packages."x86_64-linux".devShell;

#   # nix flake check is broken for these when run on x86_64-linux
#   # packages.x86_64-darwin = import ./pkgs/default.nix (args // { system = "x86_64-darwin"; });
#   # devShells.x86_64-darwin.default = self.packages."x86_64-darwin".devShell;
#   packages.aarch64-darwin = import ./pkgs/default.nix (args // { system = "aarch64-darwin"; });
#   devShells.aarch64-darwin.default = self.packages."aarch64-darwin".devShell;
# };
