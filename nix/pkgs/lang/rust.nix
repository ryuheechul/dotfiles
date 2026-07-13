{ pkgs }:

let
  ifLsp = import ./lsp.nix { inherit pkgs; };
in
with pkgs;
[
  evcxr # An evaluation context for Rust
  ## - I found that using devenv is the most smooth experience with Rust via https://nixos.wiki/wiki/Rust on Nix
  ## - An alternative to uncomment packages below would be
  ##   use `export MY_NIX_EXTRA_DEVENV=1` instead and configure per project via `devenv init`
  # cargo
  # rustc
]
++ ifLsp [
  rust-analyzer # Language server for the Rust language
]
