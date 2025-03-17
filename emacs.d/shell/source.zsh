# detect the absence of eterm-color on darwin and install it
if uname | xargs test "Darwin" =; then
  /usr/bin/infocmp eterm-color &> /dev/null \
    || ~/.config/dfs-rhc/bin/darwin/terminfo-eterm-color.sh
fi

# fail fast
test -z "${INSIDE_DOOM_EMACS}" &&
  test -z "${INSIDE_EMACS_RUN_CMD_ON_START_UP}" &&
  return

# continue after the condition is met above

source "${my_dot_d}/emacs.d/doom.d/shell/source.zsh"
