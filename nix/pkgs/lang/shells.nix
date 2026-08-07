{ pkgs }:

with pkgs;
let
  ifLsp = import ./lsp.nix { inherit pkgs; };
in
[
  shellcheck # Shell script analysis tool
  shfmt # Shell parser and formatter
] ++ ifLsp [
  bash-language-server # Language server for Bash
]
