#!/usr/bin/env bash

default="nixpkgs=nixos:nixos-hardware"
src="${1:-${default}}"

pushd "$(dirname "$0")" > /dev/null

function item()
{
  if echo "${1}" |  grep '=' > /dev/null; then
    name="$(echo "${1}" | cut -d "=" -f 1)"
    pkg="$(echo "${1}" | cut -d "=" -f 2)"
    echo "{ src = \"${pkg}\"; name = \"${name}\"; } "
  else
    echo "{ src = \"${1}\"; }"
  fi
}

function construct()
{
  echo "["
  for chunk in $(echo "${src}" | tr ':' '\n')
  do
    item $chunk
  done
  echo "]"
}

params="$(construct | tr '\n' ' ')"

nix eval --impure --expr "import ./nix-path-for-paths.nix ${params}" | xargs
