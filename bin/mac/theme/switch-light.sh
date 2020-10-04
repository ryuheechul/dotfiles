#!/usr/bin/env bash

cached_result="$(cat ~/.cache/sunshine-result)"

switch-with-tmux () {
  tmux new-session -d -s light-switch
  sleep 1
  tmux send-keys -t light-switch $1 ENTER;
  # somehow waiting this much is important
  sleep 7
  tmux kill-session -t light-switch
}

if [ "$cached_result" = "day" ]; then
  switch-with-tmux 'light'
else
  switch-with-tmux 'dark'
fi
