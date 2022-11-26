if test -z "${NVIM_LISTEN_ADDRESS}" || test -z "${commands[nvr]}"; then
  # only proceed for terminals inside neovim
  return
fi

# so that fzf just within the terminal instead of using a new temp pane in tmux
unset TMUX TMUX_PANE

# simply close the window instead of exiting
# which bringing back the shell much faster
# use `exit` to exit
alias q="clear && nvr --remote-send '<esc>:q<cr>'"

# actually `nvr --remote-tab` is better experience than `floaterm`
if test -n "${FLOATERM}" && test -n "${commands[floaterm]}"; then
  export EDITOR='floaterm'
fi

# so overriding for now
export EDITOR='nvr --remote-tab'

alias nvim='nvr --remote-tab'
