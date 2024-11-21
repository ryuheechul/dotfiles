# this is a improvement over ../nix-index-database
# which simplifies one's code to use a flake without having to write a flake
# however I couldn't figure out how to propagate nixpkgs from the caller
# because the flakeCompat only accepts flakeSrc and nothing more like which nixpkgs to propagate
# which should be possible if I fork flakeCompat and modify to include that flexibility
# but I will leave that for another day
let
  sources = import ../via-niv; # see `../via-niv` on how to manage pinning (flake or none flake) repos
  flakeSrc = sources.nix-index-database;
  flakeCompat = sources.flake-compat;
  recipe = import flakeCompat { src = flakeSrc; };
  result = recipe.defaultNix;
in
result.hmModules.nix-index

# NOTE: now there is even simpler version at `../alt-nid`
