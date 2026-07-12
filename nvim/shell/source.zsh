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
# ($EDITOR is already `nvimclient` - inherited from the host nvim's env,
# boot/misc.lua - and zshrc only defaults EDITOR, never clobbers)
# for shell
alias nvim='nvr --remote-tab'

# reshape the global `e` (../../zsh/my_addons/aliases): no args = the
# host nvim's own picker instead of fzf (same as <Space>ff)
function e {
  if (( $# )); then
    ${=EDITOR} "$@"
  else
    nvr -c 'Telescope file_browser'
  fi
}

# EMACS_SOCKET_NAME = an ancestor emacs spawned this nvim
# (prep-env-for-term). A -nw/-t client frame here would be rendered,
# transitively, by the very emacs it connects to - one event loop
# feeding itself = freeze. So no tty frame: plain -n visits open in a
# fresh workspace (doom tab) of the ancestor (its `server-window',
# term-enhance/server-window-workspace) and q there returns; no `-a ''`
# fallback on purpose - if the ancestor died, erroring beats spawning a
# stray daemon
if test -n "${EMACS_SOCKET_NAME}"; then
  # `emacs` needs no own override: the global `alias emacs='emacsclient'`
  # (../../zsh/my_addons/aliases) keeps expanding into this one
  alias emacsclient='command emacsclient -n'
fi

# try to maintain clean state for devenv
test -z "${DEVENV_PROFILE}" || { cd ~ && cd - }
