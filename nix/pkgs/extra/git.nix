{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
in
  with pkgs;
  []
  ++ lib.optionals (checkEnv "MY_NIX_EXTRA_GIT")
  [
    git-lfs # git extention for large file storage
    pre-commit # for managing multi-language pre-commit hooks
  ]
