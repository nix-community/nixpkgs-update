{
  description = "update nixpkgs automatically";

  inputs.mmdoc.url = "github:ryantm/mmdoc";
  inputs.mmdoc.inputs.nixpkgs.follows = "nixpkgs";

  nixConfig.extra-substituters = "https://nixpkgs-update.cachix.org";
  nixConfig.extra-trusted-public-keys = "nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8=";

  outputs = { self, nixpkgs, mmdoc } @ args:
    {
      packages.x86_64-linux = import ./pkgs/default.nix (args // { system = "x86_64-linux"; });
      devShell.x86_64-linux = self.packages."x86_64-linux".devShell;
      packages.x86_64-darwin = import ./pkgs/default.nix (args // { system = "x86_64-darwin"; });
      devShell.x86_64-darwin = self.packages."x86_64-darwin".devShell;
    };
}
