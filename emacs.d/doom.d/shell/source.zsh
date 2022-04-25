# fail fast if the condition is not met
if test "${INSIDE_EMACS}" = 'vterm' \
  && test -n "${EMACS_VTERM_PATH}" \
  && test -f "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"
then
	true
else
	return
fi

# optional integration for https://github.com/akermu/emacs-libvterm#shell-side-configuration-files
source "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"

# these two must be set from emacs prior to launching terminal from emacs
# export INSIDE_DOOM_EMACS=1
# export FORCE_LOAD_MY_ZSH_STUFF=1

# any others can be set here
export COLORTERM=truecolor
export TERMINFO_DIRS="$(nix-outpath ncurses)/share/terminfo"

# TODO: while replacing `e` is good `vi` is actually confusing so let me think about it
alias vi='vterm_cmd find-file'
alias e='vi $(fzf)'

# make `light` and `dark` to work with emacs as well
unalias light
light() {
	base16_solarized-light
  vterm_cmd switch-theme "base16-solarized-light"
  # to cooperate with ../../../bin/local/base16-shell-auto-reload-on-tmux
  date +s > ~/.base16_theme.updated-time
}

# TODO: dry these two functions into one?
unalias dark
dark() {
	base16_solarized-dark
  vterm_cmd switch-theme "base16-solarized-dark"
  # to cooperate with ../../../bin/local/base16-shell-auto-reload-on-tmux
  date +s > ~/.base16_theme.updated-time
}

test "${DOOM_EMACS_THEME}" = "base16-solarized-dark" && dark || light

echo "shell overriding for doom emacs has been completed."
