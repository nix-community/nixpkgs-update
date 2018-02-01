# nix-update

git ls-remote --refs --tags git://github.com/mattermost/mattermost-server.git | grep -v "rc" | tail -1

nix-instantiate -I nixpkgs=/home/ryantm/p/nixpkgs --eval --expr 'let pkgs = import <nixpkgs> {}; in builtins.elemAt pkgs.mattermost.src.urls 0'

nix-instantiate -I nixpkgs=/home/ryantm/p/nixpkgs --eval --expr 'let pkgs = import <nixpkgs> { config = {permittedInsecurePackages = [ "autotrace-0.31.1" ];}; }; aV = builtins.attrValues pkgs; in map (v: builtins.elemAt v.src.urls 0) (builtins.filter (builtins.hasAttr "src") (builtins.filter builtins.isAttrs aV))'
