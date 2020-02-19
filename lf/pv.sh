#!/usr/bin/env bash

unset COLORTERM

filename=$(basename $1)
EXT="${filename##*.}"

# lowercasing
# if this doesn't work check if you bash is old like 3.x on like on macOS
# then `$ brew install bash`
ext="${EXT,,}"

if [ $ext == "md" ]; then
  echo '`# Markdown: '"$1"'`via **glow**' | glow - -s dark
  glow -s dark "$1"
else
  bat --color=always --theme=base16 "$@"
fi
