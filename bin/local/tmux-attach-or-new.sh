#!/usr/bin/env bash

if [[ -z "$1" ]]; then
  echo "you will need to provide a session name. Try \`$0 [my-session-name]\`."
  exit 1
fi

# enable nix for the rest of script to prevent using system's old tmux
. ~/.nix-profile/etc/profile.d/nix.sh

SHELL=$(which zsh)
session_name="$1"
# recommended via https://stackoverflow.com/a/3432749
tmux a -t "${session_name}" || tmux new -s "${session_name}"
