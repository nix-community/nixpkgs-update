# Installation {#installation}

For the Cachix cache to work, your user must be in the trusted-users
list or you can use sudo since root is effectively trusted.

Run without installing:

```bash
nix run \
  --option extra-substituters 'https://nixpkgs-update.cachix.org/' \
  --option trusted-public-keys 'nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8=' \
  -f https://github.com/ryantm/nixpkgs-update/archive/master.tar.gz \
  -c nixpkgs-update --help
```

Install into your Nix profile:

```bash
nix-env \
  --option extra-substituters 'https://nixpkgs-update.cachix.org/' \
  --option trusted-public-keys 'nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8=' \
  -if https://github.com/ryantm/nixpkgs-update/archive/master.tar.gz
```

Declaratively with [niv](https://github.com/nmattia/niv):

```bash
niv add ryantm/nixpkgs-update
```

NixOS config with Niv:

```nix
let
  sources = import ./nix/sources.nix;
  nixpkgs-update = import sources.nixpkgs-update {};
in
  environment.systemPackages = [ nixpkgs-update ];
```

home-manager config with Niv:

```nix
let
  sources = import ./nix/sources.nix;
  nixpkgs-update = import sources.nixpkgs-update {};
in
  home.packages = [ nixpkgs-update ];
```
