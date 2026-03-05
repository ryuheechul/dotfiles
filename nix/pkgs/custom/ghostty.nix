{ pkgs, ... }:

# inspired by ./alt-nid
let
  sources = import ./via-niv;
  flake = sources.ghostty.url;
  result = builtins.getFlake flake;
in
result.packages.${pkgs.stdenv.hostPlatform.system}.ghostty
