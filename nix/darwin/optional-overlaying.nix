let
  unstable = import <nixpkgs> {};
  stdenv = unstable.stdenv;
  lib = unstable.lib;
in
  {
    overlays = [] ++ lib.optionals stdenv.isDarwin [ (import ./overlay.nix) ];
  }
