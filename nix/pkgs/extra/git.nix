{ pkgs ? import <nixpkgs> (import ./fallback/darwin/optional-overlaying.nix) }:

with pkgs;
[
  git-lfs # git extention for large file storage
  pre-commit # for managing multi-language pre-commit hooks
]
