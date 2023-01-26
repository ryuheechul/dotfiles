{ pkgs }:

with pkgs;
[ ]
## I found that using devenv is the most smooth experience with Rust from https://nixos.wiki/wiki/Rust
## so use MY_NIX_EXTRA_DEVENV instead and configure per project via `devenv init`
# [
#   rustup # The Rust toolchain installer
# ]
