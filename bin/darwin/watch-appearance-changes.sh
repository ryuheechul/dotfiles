#!/usr/bin/env bash

# call this from An app/process that runs on start up - https://stackoverflow.com/a/6445525/1570165
# by copying the command below
# ~/.config/dfs-rhc/bin/darwin/watch-appearance-changes.sh

# `timeout` is a part of GNU Coreutils - https://www.gnu.org/software/coreutils/manual/coreutils.html#timeout-invocation
# Without it, all of the commands in this script will be in the same group - meaning killing one (by `single-daemon`) will kill all
# Which is a undesired behavior with `single-daemon`
# therefore use timeout to have a side effect of having separate process group id
# This method was discovered at https://unix.stackexchange.com/a/403193/396504

export FORCE_LOAD_MY_ZSH_STUFF=1
exec zsh -lc '
source ~/.zshrc
timeout 5s single-daemon base16-shell-auto-reload-on-tmux
timeout 5s single-daemon ~/.config/dfs-rhc/bin/darwin/base16-shell-to-follow-system-appearance.sh
'