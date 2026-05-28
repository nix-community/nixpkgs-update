# Installation {#installation}

Run without installing on stable Nix:

```ShellSession
$ nix run \
  -f https://github.com/nix-community/nixpkgs-update/archive/main.tar.gz \
  -c nixpkgs-update --help
```

Run without installing on unstable Nix with nix command enabled:

```ShellSession
$ nix shell \
  -f https://github.com/nix-community/nixpkgs-update/archive/main.tar.gz \
  -c nixpkgs-update --help
```

Run without installing on unstable Nix with nix flakes enabled:

```ShellSession
$ nix run \
  github:nix-community/nixpkgs-update -- --help
```

Install into your Nix profile:

```ShellSession
$ nix-env \
  -if https://github.com/nix-community/nixpkgs-update/archive/main.tar.gz
```

Declaratively with [niv](https://github.com/nmattia/niv):

```ShellSession
$ niv add nix-community/nixpkgs-update
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
