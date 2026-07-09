if test -z "${NVIM_LISTEN_ADDRESS}" || test -z "${commands[nvr]}"; then
  # only proceed for terminals inside neovim
  return
fi

# so that fzf just within the terminal instead of using a new temp pane in tmux
unset TMUX TMUX_PANE

# simply close the window instead of exiting
# which bringing back the shell much faster
# use `exit` to exit
# -c (an Ex command over RPC), NOT --remote-send: sent keys replay through
# the host's mappings, and ':' no longer enters cmdline there (remapped to
# a Telescope picker by the anti-shift keybindings) - so the old
# '<esc>:q<cr>' opened that picker with "q" as the filter instead
alias q="clear; nvr -c quit"

## use host nvim instead of creating another nvim process
# for programs like lf
export EDITOR='editor-in-nvim'
# for shell
alias nvim='nvr --remote-tab'

# try to maintain clean state for devenv
test -z "${DEVENV_PROFILE}" || { cd ~ && cd - }
