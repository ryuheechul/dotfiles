{ pkgs }:

with pkgs;
[
  nil # Yet another language server for Nix
  nixpkgs-fmt # Nix code formatter for nixpkgs
  niv # Easy dependency management for Nix projects
]

# regarding niv:
# - https://lucperkins.dev/blog/nix-channel/
# - https://www.youtube.com/watch?v=atmoYyBAhF4
