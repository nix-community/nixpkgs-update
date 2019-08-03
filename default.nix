{ nixpkgs-tarball ? builtins.fetchTarball {
  name = "nixpkgs-unstable";
  url = "https://releases.nixos.org/nixos/unstable/nixos-19.09pre186563.b5f5c97f7d6/nixexprs.tar.xz";
  sha256 = "175jkhcfdyq0ddvc0188dzpm9lgmrplvgxx5gzmbzy86jywhhqs2";
}, pkgs ? import nixpkgs-tarball { config = { allowBroken = true; }; } }:

let

  compiler = pkgs.haskell.packages."ghc865";

  inherit (pkgs.haskell.lib) dontCheck doJailbreak;

  pkg = compiler.developPackage {
    root = ./.;
    overrides = self: super: {
      time-compat = dontCheck super.time-compat;
      binary-orphans = dontCheck super.binary-orphans;
      binary-instances = dontCheck super.binary-instances;
      hpack = dontCheck super.hpack;
    };
    source-overrides = {
      aeson = "1.4.3.0";
      time-compat = "1.9.2.2";
      binary-orphans = "1.0.1";
      first-class-families = "0.5.0.0";
      th-abstraction = "0.3.1.0";
      th-lift = "0.8.0.1";
    };
  };

  buildInputs = with pkgs; [ nix gitAndTools.hub jq tree gist getent ];

in pkg.overrideAttrs (attrs: {
  buildInputs = attrs.buildInputs ++ buildInputs;
  })
