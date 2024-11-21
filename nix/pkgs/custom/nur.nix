{ pkgs, ... }:
# see a usage example at ../../nixos/recipes/waydroid.nix
let
  # to support NUR https://github.com/nix-community/NUR;
  # manage pinning with https://github.com/nmattia/niv instead of a manual way from https://github.com/nix-community/NUR?tab=readme-ov-file#pinning;
  sources = import ./via-niv;
  nur = import sources.nur { inherit pkgs; };
in
nur
