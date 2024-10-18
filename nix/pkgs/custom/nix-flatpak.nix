# https://github.com/gmodena/nix-flatpak/tree/main
# see a usage at ../../nixos/recipes/flatpak.nix
let
  rev = "8d1193a959c7810f01610c10a055b0020390bf4e";
  url = "https://github.com/gmodena/nix-flatpak/archive/${rev}.tar.gz";
  path = "/modules/nixos.nix";
  fullPath = (fetchTarball url) + path;
in
import fullPath
