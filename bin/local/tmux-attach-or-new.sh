#!/usr/bin/env bash

if [[ -z "$1" ]]; then
  echo "you will need to provide a session name. Try \`$0 [my-session-name]\`."
  exit 1
fi
session_name="$1"
# recommended via https://stackoverflow.com/a/3432749
tmux a -t "${session_name}" || tmux new -s "${session_name}"
