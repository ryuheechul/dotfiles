#!/usr/bin/env bash

# call this from An app/process that runs on start up - https://stackoverflow.com/a/6445525/1570165
# by copying the command below
# ~/.config/dfs-rhc/bin/darwin/watch-appearance-changes.sh

export FORCE_LOAD_MY_ZSH_STUFF=1
zsh -lc 'source ~/.zshrc; single-daemon base16-shell-auto-reload-on-tmux; single-daemon ~/.config/dfs-rhc/bin/darwin/base16-shell-to-follow-system-appearance.sh'
