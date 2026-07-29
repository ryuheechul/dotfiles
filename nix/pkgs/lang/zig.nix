{ pkgs }:

let
  ifLsp = import ./lsp.nix { inherit pkgs; };
in
with pkgs;
[
  zig # General-purpose programming language and toolchain for maintaining robust, optimal, and reusable software
]
++ ifLsp [
  zls # Zig LSP implementation + Zig Language Server
]
