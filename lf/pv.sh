#!/usr/bin/env bash

lf_d="${XDG_CONFIG_HOME}/lf"
less_sh="${lf_d}/less.sh"

# if rsop is discovered, augment previewing with rsop
if command -v rsop >/dev/null; then
  if test -n "${LF_PV_WITH_PAGER}"; then
    RSOP_MODE=preview COLUMNS="$2" LINES="$3" exec rsop "$1" | ${less_sh}
  else
    RSOP_MODE=preview COLUMNS="$2" LINES="$3" exec rsop "$1"
  fi
# if not fallback to my own legacy script
else
  if test -n "${LF_PV_WITH_PAGER}"; then
    "${lf_d}/pv-legacy.sh" "$@" | ${less_sh}
  else
    "${lf_d}/pv-legacy.sh" "$@"
  fi
fi
