{ pkgs }:

let
  ifLsp = import ./lsp.nix { inherit pkgs; };
in
with pkgs;
[
  swift # Swift Programming Language
]
++ ifLsp [
  sourcekit-lsp # Language Server Protocol implementation for Swift and C-based languages
]
