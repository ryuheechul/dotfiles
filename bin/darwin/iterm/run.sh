#!/usr/bin/env bash

# use this script if the command takes a bit of time like ssh

cmd="${1}"

echo "running ${cmd}..."
${cmd}

# Example Usage
# put this line to Command on an iTerm's profile
# ~/dotfiles/bin/mac/iterm/run.sh 'ssh xxx'
