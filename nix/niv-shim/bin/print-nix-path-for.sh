#!/usr/bin/env bash

src="${1:-nixpkgs}"

pushd "$(dirname "$0")" > /dev/null

source ../../bin/source/config.sh

nix eval --impure --expr "import ./nix-path.nix {src=\"${src}\";}" | xargs
