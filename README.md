# nix-update

Currently interesting files `ups.sh, up.sh, check-result.sh`.

1. get a list of oudated packages

```
    git clone https://github.com/ryantm/repology-api.git
    cabal2nix --shell --hpack . > shell.nix && nix-build shell.nix && result/bin/repology-api > packages-to-update.txt
```
2. Prepare a nixpkgs git repository where you want to do the updates. It expects origin to be where you want to push your updates. It clobbers any changes in that directory with `git reset --hard`. USE THIS ON A NEWLY CLONED REPO ONLY FOR THIS PURPOSE.
3. copy list of ones you want to update into `ups.sh` ARGUMENTS variable
4. delete update arguments that seem wrong. (Weird version strings, versions that don't seem like an update, the update version doesn't look like it would work with nixpkgs)
5. Be in the directory of your update git repository. run `ups.sh` if it succeeds, it will make commits and push updates to the origin remote!
