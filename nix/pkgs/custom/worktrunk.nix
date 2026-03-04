{ pkgs }:

# for https://github.com/max-sixty/worktrunk

let
  sources = import ./via-niv;
  flake = sources.worktrunk.url;
  result = builtins.getFlake flake;
in
result.packages.${pkgs.system}.default
