{ nixpkgs-tarball ? builtins.fetchTarball {
  name = "nixpkgs-unstable";
  url = "https://releases.nixos.org/nixos/unstable/nixos-19.09pre182062.1dc26c32edf/nixexprs.tar.xz";
  sha256 = "1rdci8ghq0vi404m2qnd45bacx3sgym30whvlm23w7hr6hzyhh9x";
}, pkgs ? import nixpkgs-tarball {} }:

let

  hp = pkgs.haskellPackages.extend (pkgs.haskellPackages.packageSourceOverrides {
    nixpkgs-update = ./.;
  });

  runtimeDeps = with pkgs; [ gitAndTools.hub jq tree gist ];

  drv = pkgs.buildEnv {
    name = "nixpkgs-update-env";
    paths = [ hp.nixpkgs-update ] ++ runtimeDeps;
  };

  sh = hp.shellFor {
    packages = p: [p.nixpkgs-update];
    buildInputs = [ hp.ghcid ] ++ runtimeDeps;
  };

in
  if pkgs.lib.inNixShell then sh else drv
