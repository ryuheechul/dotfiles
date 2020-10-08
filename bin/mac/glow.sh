#!/usr/bin/env bash

theme=$(printf '%s' "${BASE16_THEME}" | sed 's/solarized-//')

glow -p -s "$theme" "$@"
