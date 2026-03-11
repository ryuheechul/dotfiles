{ pkgs }:

# Keep 1.x version due to this issue on the recent major version upgrade: https://github.com/cachix/devenv/issues/2576
let
  sources = import ../../niv-shim/nix/sources.nix;
  nixpkgs-nixos = import sources.nixos {
    inherit (pkgs.stdenv.hostPlatform) system;
  };
in
nixpkgs-nixos.devenv
