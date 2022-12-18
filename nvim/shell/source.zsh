if test -z "${NVIM_LISTEN_ADDRESS}" || test -z "${commands[nvr]}"; then
  # only proceed for terminals inside neovim
  return
fi

# so that fzf just within the terminal instead of using a new temp pane in tmux
unset TMUX TMUX_PANE

# simply close the window instead of exiting
# which bringing back the shell much faster
# use `exit` to exit
alias q="clear; nvr --remote-send '<esc>:q<cr>'"

## use host nvim instead of creating another nvim process
# for programs like lf
export EDITOR='nvr -c "lua require(\"utils.trap-close-for-term\")()" --remote-tab-wait'
# for shell
alias nvim='nvr -c "lua require(\"utils.trap-close-for-term\")()" --remote-tab-wait'
