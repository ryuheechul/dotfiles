{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
  devenv = import ../custom/devenv.nix;
in
with pkgs;
[ ]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_GIT")
  [
    git-lfs # git extention for large file storage
    pre-commit # for managing multi-language pre-commit hooks
    gitwatch # Watch a file or folder and automatically commit changes to a git repo easily.
  ]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_DEVENV")
  # devenv is not a language but help a quick setup of a dev environment for many languages
  # - https://devenv.sh/languages/
  # - https://github.com/cachix/devenv/blob/main/examples/supported-languages/devenv.nix
  [
    devenv
  ]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_EXERCISM")
  [
    exercism # CLI for exercism.org
  ]
