{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
  tag = import ../custom/tag {pkgs=pkgs;};
in
  with pkgs;
  []
  ++ lib.optionals (checkEnv "MY_NIX_EXTRA_GIT")
  [
    git-lfs # git extention for large file storage
    pre-commit # for managing multi-language pre-commit hooks
  ]
  ++ lib.optionals (checkEnv "MY_NIX_EXTRA_GO")
  [
    go # golang
    gopls # go language server
    gocode # auto completion for go
    gotools # updates your Go import lines, adding missing ones and removing unreferenced ones
    gotags # ctags-compatible tag generator for Go
    delve # go debugger
  ]
  ++ lib.optionals (checkEnv "MY_NIX_EXTRA_AWS")
  [
    # also consider installing `pipx install aws-shell`
    awscli2 # aws cli
    ssm-session-manager-plugin # AWS SSM Plugin
    amazon-ecs-cli # aws ecs cli
  ]
  ++ lib.optionals (checkEnv "MY_NIX_EXTRA_CI")
  [
    circleci-cli # circle ci cli # add checkEnv MY_NIX_EXTRA_CIRCLE_CI
  ]
  ++ lib.optionals (checkEnv "MY_NIX_EXTRA_TAG")
  [
    tag
  ]
  ++ lib.optionals (checkEnv "MY_NIX_EXTRA_EXERCISM")
  [
  exercism # CLI for exercism.org
  ]
  # # this is actually not working great at least on ubuntu
  # # it's probably wise to follow https://tailscale.com/kb/1031/install-linux/ instead
  # ++ lib.optionals (checkEnv "MY_NIX_EXTRA_TAILSCALE")
  # [
  #   tailscale
  # ]
