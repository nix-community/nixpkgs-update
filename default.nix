{ nixpkgs-tarball ? builtins.fetchTarball {
  name = "nixpkgs-unstable";
  url = "https://releases.nixos.org/nixpkgs/nixpkgs-19.09pre173445.796a8764ab8/nixexprs.tar.xz";
  sha256 = "04frhzc74xx2zsq7gbbnnh2d24hnl58i2sd24hdbwx8kyyh386xd";
}, nixpkgs ? import nixpkgs-tarball {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./nixpkgs-update.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  nixpkgs-update = variant (haskellPackages.callPackage f {});

  parts = with pkgs; [ nixpkgs-update gitAndTools.hub jq tree gist ];

  drv = pkgs.buildEnv {
    name = "nixpkgs-update-env";
    paths = parts;
  };

  sh = pkgs.mkShell {
    name = "nixpkgs-update-shell";
    buildInputs = [ drv ];
  };

in

  if pkgs.lib.inNixShell then sh else drv
