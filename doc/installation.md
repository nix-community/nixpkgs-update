# Installation {#installation}

::: note
For the Cachix cache to work, your user must be in the trusted-users
list or you can use sudo since root is effectively trusted.
:::

Run without installing on stable Nix:

```ShellSession
$ nix run \
  --option extra-substituters 'https://nixpkgs-update.cachix.org/' \
  --option extra-trusted-public-keys 'nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8=' \
  -f https://github.com/ryantm/nixpkgs-update/archive/main.tar.gz \
  -c nixpkgs-update --help
```

Run without installing on unstable Nix with nix command enabled:

```ShellSession
$ nix shell \
  --option extra-substituters 'https://nixpkgs-update.cachix.org/' \
  --option extra-trusted-public-keys 'nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8=' \
  -f https://github.com/ryantm/nixpkgs-update/archive/main.tar.gz \
  -c nixpkgs-update --help
```

Run without installing on unstable Nix with nix flakes enabled:

```ShellSession
$ nix run \
  --option extra-substituters 'https://nixpkgs-update.cachix.org/' \
  --option extra-trusted-public-keys 'nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8=' \
  github:ryantm/nixpkgs-update -- --help
```

Install into your Nix profile:

```ShellSession
$ nix-env \
  --option extra-substituters 'https://nixpkgs-update.cachix.org/' \
  --option extra-trusted-public-keys 'nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8=' \
  -if https://github.com/ryantm/nixpkgs-update/archive/main.tar.gz
```

Declaratively with [niv](https://github.com/nmattia/niv):

```ShellSession
$ niv add ryantm/nixpkgs-update
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
