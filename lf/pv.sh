#!/usr/bin/env bash

filename="$(basename "$1")"
EXT="${filename##*.}"

# lowercasing
# if this doesn't work check if you bash is old like 3.x on like on macOS
# then `$ brew install bash`
ext="${EXT,,}"

lf_d="${HOME}/.config/lf"
glow_sh="${lf_d}/glow.sh"
less_sh="${lf_d}/less.sh"

if [ "${ext}" == "md" ]; then
  function with-glow () {
    echo '`# Markdown: '"$1"'` via **glow**' | ${glow_sh}
    ${glow_sh} "$1"
  }

  if test -n "${LF_PV_WITH_PAGER}"; then
    # in the future, glow would come with opening editor from the page
    # https://github.com/charmbracelet/glow/issues/182
    with-glow $1 | ${less_sh}
  else
    with-glow $1
  fi
else
  paging_flag=''

  if test -n "${LF_PV_WITH_PAGER}"; then
    # use bat-riffle if exist
    if test -n "$(command -v bat-riffle)"; then
      bat-riffle "${@}"; exit 0
    fi

    # basically get rid of `--quit-if-one-screen` option
    BAT_PAGER="less --RAW-CONTROL-CHARS --mouse -I"

    # this might not be necessary as `auto` might still pick up paging
    # but nothing wrong to set it explicitly
    paging_flag='--paging=always'
  fi

  bat ${paging_flag} --force-colorization "$@"
fi
