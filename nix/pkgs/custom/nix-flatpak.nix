# https://github.com/gmodena/nix-flatpak/tree/main
# see a usage at ../../nixos/recipes/flatpak.nix

# inspired by ./alt-nid/default.nix
let
  sources = import ./via-niv;
  flake = sources.nix-flatpak.url;
  result = builtins.getFlake flake;
in
result.nixosModules.nix-flatpak
