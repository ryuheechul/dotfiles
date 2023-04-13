#!/usr/bin/env bash

# run this script with nix-shell -p coreutils especially on macOS

set -x

# early exit to be "idempotent"
command -v home-manager && { echo 'home-manager already initialized'; exit 0; }

export NIX_PATH="${HOME}/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH"

nix-shell '<home-manager>' -A install

path_for_hm="${HOME}/.config/home-manager"
path_for_home_nix="${path_for_hm}/home.nix"

# print previous file content in case there's one
test -f "${path_for_home_nix}" && cat "${path_for_home_nix}"

curr_dir="$(dirname "$0")"
repo_root="${curr_dir}/../.."
repo_root_abs="$(readlink -f "${repo_root}")"
my_nix_home_path="${repo_root_abs}/nix/home"

# use this as a template to replace ~/.config/nixpkgs/home.nix
echo "generating ${path_for_home_nix}"
mkdir -p "${path_for_hm}"
cat << EOF > "${path_for_home_nix}"
# generated via init-home-manager.sh
let
  home-nix-path = /. + builtins.toPath "$(echo "${my_nix_home_path}")";
  imports = [ home-nix-path ];
in
{
  inherit imports;

  home.username = "$(whoami)";
  home.homeDirectory = "$(echo "${HOME}")";
}
EOF
