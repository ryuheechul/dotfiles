{ pkgs }:

let
  checkEnv = import ../../../utils/checkEnv.nix;
in
  with pkgs;
  []
  ++ lib.optionals (checkEnv "MY_NIX_EXTRA_GO")
  [
    go # golang
    gopls # go language server
    gocode # auto completion for go
    goimports # updates your Go import lines, adding missing ones and removing unreferenced ones
    gotags # ctags-compatible tag generator for Go
    delve # go debugger
  ]
