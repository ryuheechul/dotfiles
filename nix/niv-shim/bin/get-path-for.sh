#!/usr/bin/env bash

src="${1:-nixpkgs}"
# consider using nix to make sure this script works in any environment if necessary
# e.g. https://ertt.ca/nix/shell-scripts/

pushd "$(dirname "$0")" >/dev/null

# making sure NIX_CONFIG exist especially `nix eval requires nix-command feature`
printenv NIX_CONFIG | grep nix-command >/dev/null ||
  export NIX_CONFIG="extra-experimental-features = nix-command flakes"

# https://stackoverflow.com/a/51242645/1570165
nix eval --impure --expr "import ./alt-src-to-channel.nix \"${src}\"" | xargs
