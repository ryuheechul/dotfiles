#!/usr/bin/env bash

theme=$(current-base16 | sed 's/solarized-//')

# assume no piping
if test -n "${1}"; then
  glow -s "$theme" "$@"
else # assume piping
  glow -s "$theme" /dev/stdin
fi
