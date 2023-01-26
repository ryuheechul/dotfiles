{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
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
    # https://devenv.sh/getting-started/#__tabbed_3_3
    (import (fetchTarball https://github.com/cachix/devenv/archive/v0.5.tar.gz)).default
  ]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_EXERCISM")
  [
    exercism # CLI for exercism.org
  ]
