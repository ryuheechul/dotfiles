#!/usr/bin/env bash

script_d="$(dirname "$0")"

cat <<EOF >> "${script_d}/local-pkgs.nix"
{ pkgs }:

with pkgs;
[
]
EOF
