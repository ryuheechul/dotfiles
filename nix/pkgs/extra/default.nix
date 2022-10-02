{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
  tag = import ../custom/tag { pkgs = pkgs; };
  bat-riffle = import ../custom/bat-riffle { pkgs = pkgs; };
  tf-helper = import ../custom/tf-helper.nix { pkgs = pkgs; };
  gitwatch = import ../custom/gitwatch.nix { pkgs = pkgs; };
  cfn-lint = pkgs.python3.pkgs.cfn-lint;
in
with pkgs;
[
  bat-riffle # A proof-of-concept for a pager-as-a-library. Mainly designed for bat, and not ready for general use.
]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_GIT")
  [
    git-lfs # git extention for large file storage
    pre-commit # for managing multi-language pre-commit hooks
    gitwatch # Watch a file or folder and automatically commit changes to a git repo easily.
  ]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_GO")
  [
    go # golang
    gopls # go language server
    gocode # auto completion for go
    gotools # updates your Go import lines, adding missing ones and removing unreferenced ones
    delve # go debugger
  ]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_AWS")
  [
    # also consider installing `pipx install aws-shell`
    awscli2 # aws cli
    ssm-session-manager-plugin # AWS SSM Plugin
    amazon-ecs-cli # aws ecs cli
    cfn-lint # Checks cloudformation for practices and behaviour that could potentially be improved
  ]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_CI")
  [
    circleci-cli # circle ci cli # add checkEnv MY_NIX_EXTRA_CIRCLE_CI
  ]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_TAG")
  [
    tag
  ]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_TERRAFORM")
  [
    terraform
    nodePackages.cdktf-cli
    tf-helper
  ]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_ERLANG")
  [
    erlang # Programming language used for massively scalable soft real-time systems
    rebar3 # Erlang build tool that makes it easy to compile and test Erlang applications, port drivers and releases
    elixir # A functional, meta-programming aware language built on top of the Erlang VM
    gleam # A statically typed language for the Erlang VM
  ]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_EXERCISM")
  [
    exercism # CLI for exercism.org
  ]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_NOTCURSES")
  [
    # since qrcodegen is marked broken
    (pkgs.notcurses.override {
      qrcodegenSupport = false;
    })
  ]
++ lib.optionals (checkEnv "MY_NIX_EXTRA_BAT")
  (with bat-extras; [
    batman
    batgrep
    batdiff
    batwatch
    prettybat
  ])
++ lib.optionals (checkEnv "MY_NIX_EXTRA_WSL")
  [
    wslu # A collection of utilities for Windows 10/11 Linux Subsystems
    # which comes with wslview to enable opening a browser on Windows from terminal
    ruby # An object-oriented language for quick and easy programming
    # schasse/tmux-jump plugin requies it
  ]
  # add any package to try out (locally more permanent way than `nix-shell -p [package]`
++ lib.optionals (builtins.pathExists ./local-only.nix) (import ./local-only.nix { pkgs = pkgs; })
# # this is actually not working great at least on ubuntu
# # it's probably wise to follow https://tailscale.com/kb/1031/install-linux/ instead
# ++ lib.optionals (checkEnv "MY_NIX_EXTRA_TAILSCALE")
# [
#   tailscale
# ]
