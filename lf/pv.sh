#!/usr/bin/env bash

filename="$(basename "$1")"
EXT="${filename##*.}"

# lowercasing
# if this doesn't work check if you bash is old like 3.x on like on macOS
# then `$ brew install bash`
ext="${EXT,,}"

if [ "${ext}" == "md" ]; then
  echo '`# Markdown: '"$1"'`via **glow**' | glow - -s dark
  ~/.config/lf/glow.sh "$1"
else
  bat --force-colorization "$@"
fi
