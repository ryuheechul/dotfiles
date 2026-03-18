{ pkgs }:

let
  viaDeno = import ../../utils/viaDeno.nix;
in

viaDeno {
  pkgs = pkgs;
  pkg-name = "context-mode";
  deno-pkg-name = "npm:context-mode";
}
