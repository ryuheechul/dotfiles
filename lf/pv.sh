#!/usr/bin/env bash

filename="$(basename "$1")"
EXT="${filename##*.}"
# lowercasing
# if this doesn't work check if you bash is old like 3.x on like on macOS
# then `$ brew install bash`
ext="${EXT,,}"

lf_d="${XDG_CONFIG_HOME}/lf"
glow_sh="${lf_d}/glow.sh"
less_sh="${lf_d}/less.sh"

title ()
{
  echo '_'"${1}"'_ via **'"${2}"'**: `'"${3}"'`

---' | ${glow_sh}
}

markdown-via-glow () {
  function with-glow () {
    title Markdown glow "${1}"
    ${glow_sh} "$1"
  }

  if test -n "${LF_PV_WITH_PAGER}"; then
    # in the future, glow would come with opening editor from the page
    # https://github.com/charmbracelet/glow/issues/182
    with-glow $1 | ${less_sh}
  else
    with-glow $1
  fi
}

image-via-viu () {
  with-viu ()
  {
    title Image viu "${1}"
    viu "${1}"
  }

  if test -n "${LF_PV_WITH_PAGER}"; then
    with-viu "${1}" | ${less_sh}
  else
    with-viu "${1}"
  fi
}

rest-via-bat () {
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
}

case "${ext}" in
  md)
    markdown-via-glow "${1}"
    ;;
  jpg|jpeg|png|gif)
    image-via-viu "${1}"
    ;;
  *)
    rest-via-bat "${1}"
    ;;
esac
