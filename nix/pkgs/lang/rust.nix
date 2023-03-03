{ pkgs }:

with pkgs;
[
  evcxr # An evaluation context for Rust
  ## - I found that using devenv is the most smooth experience with Rust via https://nixos.wiki/wiki/Rust on Nix
  ## - An alternative to uncomment packages below would be
  ##   1. use `export MY_NIX_EXTRA_DEVENV=1` instead and configure per project via `devenv init`
  ##   2. or read ../../../bin/path/tea/README.md to use tea
  # cargo
  # rustc
]
