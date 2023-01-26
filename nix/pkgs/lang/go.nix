{ pkgs }:

with pkgs;
[
  go # golang
  gopls # go language server
  gocode # auto completion for go
  gotools # updates your Go import lines, adding missing ones and removing unreferenced ones
  delve # go debugger
]
