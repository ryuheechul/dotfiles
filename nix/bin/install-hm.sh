#!/usr/bin/env bash

# install home-manager (and upgrade if the installed one is falling behind of ../niv-shim/nix/sources.json)

curr_dir="$(dirname "$0")"
repo_root="${curr_dir}/../.."
nix_d="${repo_root}/nix"

"${nix_d}/bin/nix-shell.sh" '<home-manager>' -A install
