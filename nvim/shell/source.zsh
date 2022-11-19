# fail fast
if test -z "${NVIM_LISTEN_ADDRESS}" || test -z "${commands[nvr]}"; then
  return
fi

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
