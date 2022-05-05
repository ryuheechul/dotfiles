# fail fast if the condition is not met
if test "${INSIDE_EMACS}" = 'vterm' \
  && test -n "${EMACS_VTERM_PATH}" \
  && test -f "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"
then
	true
else
	return
fi

# https://unix.stackexchange.com/a/115431/396504
script_d=${0:a:h}

# because excluding these at `../init.el` wasn't enough when you run emacs from terminal inside tmux
for env_var in $(${script_d}/env-vars-to-exclude); do
  unset "$env_var"
done

# optional integration for https://github.com/akermu/emacs-libvterm#shell-side-configuration-files
source "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"

# these two must be set from emacs prior to launching terminal from emacs
# export INSIDE_DOOM_EMACS=1
# export FORCE_LOAD_MY_ZSH_STUFF=1

# any others can be set here
export TERM=eterm-color
export COLORTERM=truecolor
export TERMINFO_DIRS="$(nix-outpath ncurses)/share/terminfo"

# stop using these alias for now - while replacing `e` is good `vi` is actually confusing so let me think about it
# alias vi='vterm_cmd find-file'
# alias e='vi $(fzf)'

# sync in case of drift between Emacs and base16-shell
# TODO: don't run any if they are same (which will be most of the time) to enhance the performance on loading the shell
test "${DOOM_EMACS_THEME}" = "base16-solarized-dark" && dark || light

echo "shell overriding for doom emacs has been completed."
