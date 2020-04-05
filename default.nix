let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {config = { allowBroken = true; }; };

  gitignore = import sources.gitignore { inherit (pkgs) lib; };
  inherit (gitignore) gitignoreSource;

  compiler = pkgs.haskell.packages.ghc883;
  inherit (pkgs.haskell.lib) dontCheck doJailbreak overrideCabal;

  pkg = compiler.developPackage {
    name = "nixpkgs-update";
    root = gitignoreSource ./.;
    overrides = self: super: { };
    source-overrides = { };
  };

in pkg.overrideAttrs (attrs: {
  propagatedBuildInputs = with pkgs; [
    nix
    git
    getent
    gitAndTools.hub
    jq
    tree
    gist
  ];
})
