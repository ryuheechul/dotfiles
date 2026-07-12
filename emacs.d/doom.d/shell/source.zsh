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
  if ((${+functions[ghostel_cmd]})); then
    term_cmd="ghostel_cmd"
  elif [[ "${INSIDE_EMACS}" = *,tramp:* ]]; then
    # informational, not an error: the bridge only reaches a remote shell
    # when `ghostel-tramp-shell-integration` is enabled emacs-side
    >&2 echo 'ghostel_cmd bridge not pushed to this remote shell (tramp) - enable ghostel-tramp-shell-integration emacs-side if wanted'
  else
    # ghostel injects the bridge only into its direct child shell (a
    # nested shell merely inherits INSIDE_EMACS - though the [neo]vim
    # :terminal case never even gets sourced, see
    # ../../../zsh/integration/editors)
    >&2 echo 'ghostel_cmd is missing - ghostel shell integration was not injected'
  fi
  ;;
esac

if test -n "${term_cmd}"; then
  find-file() {
    # -editor-window: reuse the existing non-terminal window in the
    # frame (not this terminal's own, which bare find-file would
    # replace) - -other-window alone doesn't guarantee that, it can
    # still split a new one; see
    # ../modules/my-custom/term-enhance/{config,ghostel,vterm}.el
    "${term_cmd}" find-file-editor-window "$(realpath "${@:-.}")"
  }
  # safe over tramp too: the elisp side resolves the bare remote path
  # against the calling buffer's tramp prefix
  # (term-enhance/from-caller-fs), so the HOST's same-looking local
  # file is never opened by mistake
  alias emacs="find-file"
fi

# fix cursor shape in the shell inside the terminal - https://vim.fandom.com/wiki/Change_cursor_shape_in_different_modes
echo '\e[2 q'

if test "${term_in_emacs}" = "vterm"; then
  # this is a workaround that fixes the cursor is not changing shape properly between modes for neovim with TERM=eterm-color
  alias nvim='TERM=xterm-256color nvim'
fi
# (under ghostel, nvim needs no TERM workaround - xterm-ghostty handles cursor shapes)

# ($EDITOR is already `emacsclient` here - inherited from this emacs's
# env, set by prep-env-for-term with EMACS_SOCKET_NAME pointing at THIS
# emacs; plain client = blocking (git commit works), opened in its own
# workspace via `server-window', C-x #/q closes it and returns. tramp
# shells never see it: plain setenv doesn't reach the remote env)
if [[ "${INSIDE_EMACS}" != *tramp* ]]; then
  # a manual `emacsclient` would inherit -nw from the global alias
  # (../../../zsh/my_addons/aliases) - a tty frame of this very emacs
  # inside its own terminal freezes (one event loop feeding itself);
  # reuse the existing frame instead
  alias emacsclient='command emacsclient -n'
fi

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

# (a terminal inside [neo]vim never gets here - nvim scrubs INSIDE_EMACS
# from its :terminal environment, so ../../../zsh/integration/editors
# never sources this file there: "nearest editor wins" in
# ../../../docs/philosophy.md. nvim sets its own TERM and
# ../../../nvim/shell/source.zsh owns that shell's integration)

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

# reshape the global `vi`/`e` (../../../zsh/my_addons/aliases):
# interactive opening shouldn't block this terminal, so route through
# the non-blocking find-file bridge instead of $EDITOR (which stays
# `emacsclient` above for consumers that NEED blocking, e.g. git)
if test -n "${term_cmd}"; then
  alias vi='find-file'
  function e {
    if (( $# )); then
      find-file "$@"
    else
      "${term_cmd}" find-file-picker-editor-window "$(realpath .)"
    fi
  }
fi

# sync in case of drift between Emacs and base16-shell
# (tramp shells opt out - "One tone, every layer" in ../../../docs/philosophy.md)
echo "${INSIDE_EMACS}" | grep tramp >/dev/null ||
  {
    test "${DOOM_EMACS_THEME}" = "base16-$(current-base16)" ||
      { test "${DOOM_EMACS_THEME}" = "base16-solarized-dark" && dark || light; }
  }

# run command on start up requested from Emacs
# TODO: this technically don't need to wait for all the things above, so consider moving up there to skip extra things to wait
cmd_to_run="${INSIDE_EMACS_RUN_CMD_ON_START_UP}"
unset INSIDE_EMACS_RUN_CMD_ON_START_UP
if test -n "${cmd_to_run}"; then
  # (the quick-editor env this shell was fast-pathed with - e.g.
  # fast_shell_in_editor - deliberately rides along into the command:
  # nvim scrubs it for its own children in boot/misc.lua, "nearest
  # editor wins" in ../../../docs/philosophy.md)
  #
  # the child `zsh -c` below is non-interactive, so it sources .zshenv but
  # NOT .zshrc - which means ../../../zsh/path/set-basic's nix-profile
  # "float to front of $PATH" trick never runs there (only .zshrc's
  # `float_nix_path=1 source .../path/set` call does that; .zshenv's own
  # path/set call doesn't set that flag). without it, /opt/homebrew/bin
  # (added earlier, e.g. via path_helper) can outrank ~/.nix-profile/bin in
  # $PATH, so a command like `nvim` silently resolves to a stale Homebrew
  # install instead of the nix one - exporting float_nix_path=1 here makes
  # the child's own .zshenv -> zsh/env -> path/set float nix-profile to the
  # front, same as a normal interactive shell does
  float_nix_path=1 exec zsh -c "$cmd_to_run"
fi

echo "shell overriding for doom emacs has been completed."
