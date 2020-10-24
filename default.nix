{returnShellEnv ? false  } :
let
  flake-compat =
    (import (
      let
        lock = builtins.fromJSON (builtins.readFile ./flake.lock);
      in fetchTarball {
        url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
        sha256 = lock.nodes.flake-compat.locked.narHash; }
    ) {
      src =  ./.;
    });
in
if returnShellEnv
then flake-compat.shellNix
else flake-compat.defaultNix.default
