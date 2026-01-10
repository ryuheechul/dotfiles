{ pkgs }:

let
  viaBun = import ../../utils/viaBun.nix;
in

# declarative alternative to `bunx @google/gemini-cli`
# initially used with ../../utils/viaDeno.nix but didn't work very well (look at the history for that code)

viaBun {
  pkgs = pkgs;
  pkg-name = "gemini";
  npm-pkg-name = "@google/gemini-cli";
}
