# a helper function to access a specific version of nixpkgs as an escape hatch
# see ../../nixos/recipes/escape-hatch.nix for an example

{ rev ? "master", allowUnfree ? false }:

let
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  # https://www.reddit.com/r/NixOS/comments/1ci71ak/need_help_with_nixos_configuration_error_unfree/
  # https://www.reddit.com/r/NixOS/comments/1ci71ak/comment/l27biep/
  pkgs = (import (fetchTarball url) { config.allowUnfree = allowUnfree; });
in
pkgs
