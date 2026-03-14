#!/usr/bin/env bash

theme=$(current-base16 | sed 's/solarized-//')

# This would enable color but it would look out of style due to the issue described here until it's resolved: https://github.com/charmbracelet/glow/issues/654.
export CLICOLOR_FORCE=1
theme=dracula # Override with this for now as it looks the most acceptable with current limitation mentioned above

# Find styles at https://github.com/charmbracelet/glamour/blob/master/styles/gallery/README.md

# assume no piping
if test -n "${1}"; then
  glow -s "$theme" "$@"
else # assume piping
  glow -s "$theme" /dev/stdin
fi
