# nix-update

1. get a list of oudated packages

```
    git clone https://github.com/ryantm/repology-api.git
    cabal2nix --shell --hpack . > shell.nix && nix-build shell.nix && result/bin/repology-api > packages-to-update.txt
```
2. Prepare a nixpkgs git repository where you want to do the updates. It expects origin to be where you want to push your updates. It clobbers any changes in that directory with `git reset --hard`. USE THIS ON A NEWLY CLONED REPO ONLY FOR THIS PURPOSE.
3. copy list of ones you want to update into `ups.sh` ARGUMENTS variable
4. Be in the directory of your update git repository. run `ups.sh` if it succeeds, it will make commits and push updates to the origin remote!

# old notes

git ls-remote --refs --tags git://github.com/mattermost/mattermost-server.git | grep -v "rc" | tail -1

nix-instantiate -I nixpkgs=/home/ryantm/p/nixpkgs --eval --expr 'let pkgs = import <nixpkgs> {}; in builtins.elemAt pkgs.mattermost.src.urls 0'

nix-instantiate -I nixpkgs=/home/ryantm/p/nixpkgs --eval --expr 'let pkgs = import <nixpkgs> { config = {permittedInsecurePackages = [ "autotrace-0.31.1" ];}; }; aV = builtins.attrValues pkgs; in map (v: builtins.elemAt v.src.urls 0) (builtins.filter (builtins.hasAttr "src") (builtins.filter builtins.isAttrs aV))'


wget "https://repology.org/api/v1/metapackages/?inrepo=nix_unstable&outdated=on"
wget "https://nixos.org/nixpkgs/packages-unstable.json.gz"
