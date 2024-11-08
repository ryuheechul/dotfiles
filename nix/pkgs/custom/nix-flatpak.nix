# https://github.com/gmodena/nix-flatpak/tree/main
# see a usage at ../../nixos/recipes/flatpak.nix
let
  rev = "8d1193a959c7810f01610c10a055b0020390bf4e";
  sha256 = "sha256:1sxwpj6jdn4dcbbi21kgj5alksm44h6wxxzhb5yjv432l8vw71mr";
  url = "https://github.com/gmodena/nix-flatpak/archive/${rev}.tar.gz";
  path = "/modules/nixos.nix";
  fullPath =
    (fetchTarball {
      url = url;
      sha256 = sha256;
    })
    + path;
in
import fullPath
