{returnShellEnv ? false  } :
let
  sources = import ./nix/sources.nix;
  flake-compat = import sources.flake-compat { src = ./.; };
in
if returnShellEnv
then flake-compat.shellNix.devShell
else flake-compat.defaultNix.default
