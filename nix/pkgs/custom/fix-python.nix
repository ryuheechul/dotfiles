# inspired by ./ghostty.nix

let
  sources = import ./via-niv; # see `../via-niv` on how to manage pinning (flake or none flake) repos
  flakeSrc = sources.fix-python;
  flakeCompat = sources.flake-compat;
  recipe = import flakeCompat { src = flakeSrc; };
  result = recipe.defaultNix;
in

result.packages.${builtins.currentSystem}.default
# just for the documentation, below seems to work the same
# result.outputs.packages."${builtins.currentSystem}".default
