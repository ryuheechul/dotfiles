#!/usr/bin/env bash

src="${1:-nixpkgs}"

pushd "$(dirname "$0")" > /dev/null

nix eval --impure --expr "import ./nix-path.nix {src=\"${src}\";}" | xargs
