#!/usr/bin/env bash


filename="$(basename "$1")"

EXT="${filename##*.}"
# lowercasing
# if this doesn't work check if you bash is old like 3.x on like on macOS
# then `$ brew install bash`
ext="${EXT,,}"

lf_d="${XDG_CONFIG_HOME}/lf"
glow_sh="${lf_d}/glow.sh"

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

  with-glow $1
}

zip-via-less () {
  function with-less () {
    title Zip less "${1}"
    less "$1"
  }

  with-less $1
}

image-via-viu () {
  with-viu ()
  {
    title Image viu "${1}"
    viu "${1}"
  }

  with-viu "${1}"
}

# via tree alias from my zsh
directory-via-tree () {
  echo "@ $(realpath "${1}")"
  title Directory "tree" "${1}"
  zsh -c "tree '${1}'"
}

file-via-bat () {
  paging_flag=''
  # if test -n "${LF_PV_WITH_PAGER}"; then
  #   # use bat-riffle if exist
  #   if test -n "$(command -v bat-riffle)"; then
  #     bat-riffle "${@}"; exit 0
  #   fi
  #
  #   # basically get rid of `--quit-if-one-screen` option
  #   BAT_PAGER="less --RAW-CONTROL-CHARS --mouse -I"
  #
  #   # this might not be necessary as `auto` might still pick up paging
  #   # but nothing wrong to set it explicitly
  #   paging_flag='--paging=always'
  # fi

  bat ${paging_flag} --force-colorization "$@"
}

case "${ext}" in
  md)
    markdown-via-glow "${1}"
    ;;
  jpg|jpeg|png|gif)
    image-via-viu "${1}"
    ;;
  zip)
    zip-via-less "${1}"
    ;;
  *)
    if test -d "${1}"; then
      directory-via-tree "${1}"
    else
      file-via-bat "${@}"
    fi
    ;;
esac
