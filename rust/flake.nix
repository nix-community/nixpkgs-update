{
  description = "update nixpkgs automatically";

  outputs = { self, nixpkgs } @ args: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in {
    devShells.x86_64-linux.default = pkgs.mkShell {
      packages = [
        pkgs.cargo
        pkgs.clippy
        pkgs.sqlite
        pkgs.diesel-cli
      ];
    };
  };
}
