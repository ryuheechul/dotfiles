#!/usr/bin/env bash

theme=$(current-base16 | sed 's/solarized-//')

glow -p -s "$theme" "$@"
