#!/usr/bin/env bash

src="${1:-nixpkgs}"
# consider using nix to make sure this script works in any environment if necessary
# e.g. https://ertt.ca/nix/shell-scripts/

curr_d="$(dirname "$0")"
niv_shim_d="${curr_d}/.."
path_for_alt="${niv_shim_d}/alt-src-to-channel.nix"
echo "$(nix-instantiate --eval -E "(import ${path_for_alt} \"${src}\").outPath" | xargs echo)"
