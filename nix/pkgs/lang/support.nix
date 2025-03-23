{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
  ifEnv = envName: pkgs.lib.optionals (checkEnv envName);
  gitwatch = import ../custom/gitwatch.nix { pkgs = pkgs; };
in
with pkgs;
[
  efm-langserver # General purpose Language Server
  emacs-lsp-booster # Emacs LSP performance booster - ../../../emacs.d/doom.d/modules/tools/lsp-support
]
++ ifEnv "MY_NIX_EXTRA_GIT" [
  git-lfs # git extention for large file storage
  pre-commit # for managing multi-language pre-commit hooks
  gitwatch # Watch a file or folder and automatically commit changes to a git repo easily.
  # these below are technically not directly related to git - so maybe one day I will make its own category
  src-cli # Sourcegraph CLI
  comby # Tool for searching and changing code structure
]
++
  ifEnv "MY_NIX_EXTRA_DEVENV"
    # devenv is not a language but help a quick setup of a dev environment for many languages
    # - https://devenv.sh/languages/
    # - https://github.com/cachix/devenv/blob/main/examples/supported-languages/devenv.nix
    [
      devenv # Fast, Declarative, Reproducible, and Composable Developer Environments
      devbox # same goal as devenv but approaches are different
    ]
++ ifEnv "MY_NIX_EXTRA_EXERCISM" [
  exercism # CLI for exercism.org
]
