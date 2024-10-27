{ pkgs }:

with pkgs;
[
  nixd # Feature-rich Nix language server interoperating with C++ nix
  nil # Yet another language server for Nix
  # nixpkgs-fmt # Nix code formatter for nixpkgs - deprecated and now there is an official formatter below
  nixfmt-rfc-style # Official formatter for Nix code
  niv # Easy dependency management for Nix projects
]

# regarding niv:
# - https://lucperkins.dev/blog/nix-channel/
# - https://www.youtube.com/watch?v=atmoYyBAhF4
