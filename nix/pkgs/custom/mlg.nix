# see a usage example at ../../nixos/recipes/mlg.nix
let
  # to support NUR https://github.com/ryuheechul/mimic-launchpad-gesture/
  # manage pinning with https://github.com/nmattia/niv
  sources = import ./via-niv;

  # unlike ./nur.nix which is the list of packages, this is a module so it's slightly different that how we extract from niv
  mlgModule = import sources.mlg;
in
mlgModule
