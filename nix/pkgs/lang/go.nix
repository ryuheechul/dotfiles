{ pkgs }:

let
  ifLsp = import ./lsp.nix { inherit pkgs; };
in
with pkgs;
[
  go # golang
  gocode # auto completion for go
  gotools # updates your Go import lines, adding missing ones and removing unreferenced ones
  delve # go debugger
]
++ ifLsp [
  gopls # go language server
]
