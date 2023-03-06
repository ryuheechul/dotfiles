#!/usr/bin/env bash

# run this script with nix-shell -p coreutils especially on macOS

set -x

curr_dir="$(dirname "$0")"
repo_root="${curr_dir}/../.."
repo_root_abs="$(readlink -f "${repo_root}")"
nix_home_path="${repo_root_abs}/nix/home"

export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH

nix-shell '<home-manager>' -A install

# use this as a template to replace ~/.config/nixpkgs/home.nix
cat << EOF > ~/.config/nixpkgs/home.nix
let
  home-nix-path = /. + builtins.toPath "$(echo "${nix_home_path}")";
  imports = [ home-nix-path ];
in
{
  inherit imports;

  home.username = "$(whoami)";
  home.homeDirectory = "$(echo "${HOME}")";
}
EOF
