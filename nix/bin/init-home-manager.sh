#!/usr/bin/env bash

set -x

curr_dir="$(dirname "$0")"
repo_root="${curr_dir}/../.."
repo_root_abs="$(readlink -f "${repo_root}")"
nix_home_path="${repo_root_abs}/nix/home.nix"

nix-shell '<home-manager>' -A install

# use this as a template to replace ~/.config/nixpkgs/home.nix
cat << EOF > ~/.config/nixpkgs/home.nix
let
  imports = [
    $(echo "${nix_home_path}")
  ];
in
{
  inherit imports;

  home.username = "$(whoami)";
  home.homeDirectory = "$(echo "${HOME}")";
}
EOF
