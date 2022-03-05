# to fallback to a darwin channel in case there are issues on certain packages on darwin
self: super:
let
  fallback = import <nixpkgs-fallback-darwin> {};
  pkgs = fallback.pkgs;
in with pkgs;
{
  inherit starship;
  ## these two lines are examples
  # inherit hello;
  # inherit bash;
}
