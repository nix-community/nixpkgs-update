{ nixpkgs-tarball ? builtins.fetchTarball {
  name = "nixpkgs-unstable";
  url = "https://releases.nixos.org/nixpkgs/nixpkgs-19.09pre173445.796a8764ab8/nixexprs.tar.xz";
  sha256 = "04frhzc74xx2zsq7gbbnnh2d24hnl58i2sd24hdbwx8kyyh386xd";
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
