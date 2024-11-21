# this is a even simpler version than ../alt-nix-index-database via https://nix.dev/manual/nix/2.18/language/builtins#builtins-getFlake
# thanks to https://www.reddit.com/r/NixOS/comments/1ez9y69/how_to_import_a_flake_inside_a_non_flake_module/
let
  sources = import ../via-niv;
  flake = sources.nix-index-database.url;
  # flake = "github:nix-community/nix-index-database"; # this would work too but why not a pinned version that I already have
  result = builtins.getFlake flake;
in
result.hmModules.nix-index
