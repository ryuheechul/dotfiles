# https://unix.stackexchange.com/a/115431/396504
script_d="${0:a:h}"

# which terminal-in-emacs is this shell running in? ("vterm" or "ghostel";
# tramp may append ",tramp:..." so strip everything after the first comma)
term_in_emacs="${INSIDE_EMACS%%,*}"

case "${term_in_emacs}" in
vterm)
  # shell_integration_sh="${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"
  # below is to replace above to mitigate an issue
  shell_integration_sh="${script_d}/shim/emacs-vterm-zsh.zsh"
  # fail fast if the condition is not met
  if test -n "${EMACS_VTERM_PATH}" && test -f "${shell_integration_sh}"; then
    # optional integration for https://github.com/akermu/emacs-libvterm#shell-side-configuration-files
    source "${shell_integration_sh}"
    term_cmd="vterm_cmd"
  else
    >&2 echo 'Some error are expected. Make sure emacs-vterm-zsh.[z]sh is discoverable'
  fi
  ;;
ghostel)
  # nothing to source - ghostel auto-injects its own integration (OSC 7/133/2
  # tracking and the ghostel_cmd elisp bridge) before .zshrc even runs
  if (( ${+functions[ghostel_cmd]} )); then
    term_cmd="ghostel_cmd"
  else
    >&2 echo 'ghostel_cmd is missing - ghostel shell integration was not injected'
  fi
  ;;
esac

if test -n "${term_cmd}"; then
  find-file() {
    "${term_cmd}" find-file "$(realpath "${@:-.}")"
  }
  # block in case of SSH, otherwise it will try to open the clients (if there is the same path exist)
  test -z "${SSH_CONNECTION}" && alias emacs="find-file"
fi

# fix cursor shape in the shell inside the terminal - https://vim.fandom.com/wiki/Change_cursor_shape_in_different_modes
echo '\e[2 q'

if test "${term_in_emacs}" = "vterm"; then
  # this is a workaround that fixes the cursor is not changing shape properly between modes for neovim with TERM=eterm-color
  alias nvim='TERM=xterm-256color nvim'
  export EDITOR='TERM=xterm-256color nvim'
fi
# (under ghostel, nvim and EDITOR need no TERM workaround - xterm-ghostty handles cursor shapes)

# because excluding these at `../init.el` wasn't enough when you run emacs from terminal inside tmux
for env_var in $(${script_d}/env-vars-to-exclude); do
  # ghostel sets TERM & friends itself (xterm-ghostty + its bundled terminfo) -
  # those are deliberate, not outer-terminal contamination, so keep them
  # (vterm doesn't need this skip since it re-exports TERM=eterm-color below)
  if test "${term_in_emacs}" = "ghostel"; then
    case "${env_var}" in
    TERM | TERM_PROGRAM | TERM_PROGRAM_VERSION) continue ;;
    esac
  fi
  unset "$env_var"
done

# any others can be set here
export COLORTERM=truecolor
# for eterm-color lookup via nix ncurses (ghostel ships its own terminfo via $TERMINFO instead)
if test "${term_in_emacs}" = "vterm"; then
  export TERMINFO_DIRS="$(nix-outpath infocmp)/share/terminfo"
fi

# for terminal inside neovim inside the emacs terminal

# term inside neovim inside the emacs terminal
if test -n "${VIMRUNTIME}"; then
  export TERM=xterm-256color
  echo '[Info] You are in the term inside [neo]vim.'

  # stop loading the rest in case in neovim
  return
fi
# now these should be for the emacs terminal level only

if test "${term_in_emacs}" = "vterm"; then
  export TERM=eterm-color

  # lf doesn't seem to handle colors with eterm-color very well - this workaround seems to be the cleanest to approaches with color issue (especially with previewed files)
  alias lf='TERM=xterm-256color lf'
fi
# (never override TERM under ghostel - xterm-ghostty plus the injected $TERMINFO is how child apps find its capabilities)

if test -n "${term_cmd}"; then
  # elisp-side hide commands are registered per terminal: vterm/hide and ghostel/hide
  hide_fn="${term_in_emacs}/hide"
  # this way would prevent the shell to be actually closed and open again needlessly which makes toggle feel prompt (after initial opening)
  # when you need an actual exit, just type `exit`
  printenv ZELLIJ || alias q="${term_cmd} ${hide_fn} && clear" # `clear` give you the illusion of opening the shell again (promptly) - don't use `clear` when debugging

  # to work in tandem with ../modules/my-custom/morevil/
  alias fg="${term_cmd} ${hide_fn}" # this override `fg` completely which is not good in case I need `fg` within emacs - TODO: fix it

  # despite its name it should only work for the "full screen one"
  "${term_cmd}" "${term_in_emacs}/unhide-mode-line"
fi

# stop using these alias for now - while replacing `e` is good `vi` is actually confusing so let me think about it
# alias vi='vterm_cmd find-file'
# alias e='vi $(fzf)'

# sync in case of drift between Emacs and base16-shell
echo "${INSIDE_EMACS}" | grep tramp >/dev/null ||
  {
    test "${DOOM_EMACS_THEME}" = "base16-$(current-base16)" ||
      { test "${DOOM_EMACS_THEME}" = "base16-solarized-dark" && dark || light }
  }

# run command on start up requested from Emacs
# TODO: this technically don't need to wait for all the things above, so consider moving up there to skip extra things to wait
cmd_to_run="${INSIDE_EMACS_RUN_CMD_ON_START_UP}"
unset INSIDE_EMACS_RUN_CMD_ON_START_UP
test -n "${cmd_to_run}" && exec zsh -c "$cmd_to_run"

echo "shell overriding for doom emacs has been completed."
