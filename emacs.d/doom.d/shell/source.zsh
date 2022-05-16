# https://unix.stackexchange.com/a/115431/396504
script_d="${0:a:h}"

# shell_integration_sh="${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"
# below is to replace above to mitigate an issue
shell_integration_sh="${script_d}/shim/emacs-vterm-zsh.zsh"

# fail fast if the condition is not met
if test "${INSIDE_EMACS}" = 'vterm' \
  && test -n "${EMACS_VTERM_PATH}" \
  && test -f "${shell_integration_sh}"
then
  # optional integration for https://github.com/akermu/emacs-libvterm#shell-side-configuration-files
  source "${shell_integration_sh}"
else
  echo "Some error are expected. Make sure `emacs-vterm-zsh.[z]sh` is discoverable"
fi

# fix cursor shape in the shell inside vterm - https://vim.fandom.com/wiki/Change_cursor_shape_in_different_modes
echo '\e[2 q'

# this is a workaround that fixes the cursor is not changing shape properly between modes for neovim with TERM=eterm-color
# skip doing this for TUI becuase somehow it breaks the terminal graphics
test -z "${TUI_EMACS}" && alias vi='TERM=xterm-256color nvim'

# because excluding these at `../init.el` wasn't enough when you run emacs from terminal inside tmux
for env_var in $(${script_d}/env-vars-to-exclude); do
  unset "$env_var"
done

# any others can be set here
export COLORTERM=truecolor
export TERMINFO_DIRS="$(nix-outpath infocmp)/share/terminfo"

# for terminal inside neovim inside vterm

# term inside neovim inside vterm
if test -n "${VIMRUNTIME}"; then
  export TERM=xterm-256color
  echo '[Info] You are in the term inside [neo]vim.'

  # stop loading the rest in case in neovim
  return
fi
# now these should for vterm inside emacs level only

export TERM=eterm-color
# this way would prevent the shell to be actually closed and open again needlessly which makes toggle feel prompt (after initial opening)
# when you need an actual exit, just type `exit`
alias q='vterm_cmd vterm/hide && clear' # `clear` give you the illusion of opening the shell again (promptly) - don't use `clear` when debugging

# despite its name it should only work for the "full screen one"
vterm_cmd vterm/unhide-mode-line

# lf doesn't seem to handle colors with eterm-color very well - this workaround seems to be the cleanest to approaches with color issue (especially with previewed files)
alias lf='TERM=xterm-256color lf'

# stop using these alias for now - while replacing `e` is good `vi` is actually confusing so let me think about it
# alias vi='vterm_cmd find-file'
# alias e='vi $(fzf)'

# sync in case of drift between Emacs and base16-shell
test "${DOOM_EMACS_THEME}" = "base16-$(current-base16)" \
  || { test "${DOOM_EMACS_THEME}" = "base16-solarized-dark" && dark || light }

# run command on start up requested from Emacs
# TODO: this technically don't need to wait for all the things above, so consider moving up there to skip extra things to wait
cmd_to_run="${INSIDE_EMACS_RUN_CMD_ON_START_UP}"
unset INSIDE_EMACS_RUN_CMD_ON_START_UP
test -n "${cmd_to_run}" && exec zsh -c "$cmd_to_run"

echo "shell overriding for doom emacs has been completed."
