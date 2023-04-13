{ pkgs }:

let
  # system pkgs for local only
  local-only = pkgs.lib.optionals (builtins.pathExists ./system-pkgs-local.nix) (import ./system-pkgs-local.nix { pkgs = pkgs; });
in
# system pkgs for any nixOS
with pkgs; [
  vim # would you rather use nano?
  nvd # Nix/NixOS package version diff tool
  # https://www.reddit.com/r/linux_gaming/comments/ynue9u/comment/ivat383
  glxinfo # Test utilities for OpenGL - `glxinfo -B`
  glmark2 # OpenGL (ES) 2.0 benchmark
]
++ local-only
