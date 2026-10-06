#!/usr/bin/env bash

default="nixpkgs=nixos:nixos-hardware"
src="${1:-${default}}"

pushd "$(dirname "$0")" > /dev/null || exit

function item()
{
  if echo "${1}" | grep '=' > /dev/null; then
    name="$(echo "${1}" | cut -d "=" -f 1)"
    pkg="$(echo "${1}" | cut -d "=" -f 2)"
    echo "{ src = \"${pkg}\"; name = \"${name}\"; } "
  else
    echo "{ src = \"${1}\"; }"
  fi
}

function construct()
{
  echo "["
  for chunk in $(echo "${src}" | tr ':' '\n')
  do
    item "${chunk}"
  done
  echo "]"
}

params="$(construct | tr '\n' ' ')"

source ../../bin/source/config.sh

# The machinery below grew out of this original one-liner, kept here as a
# reference for what the complexity exists to support:
#
#   nix eval --impure --expr "import ./nix-path-for-paths.nix ${params}" | xargs
#
# The first shell after boot can start before nix-daemon's socket is
# listening (about 1-4s; a stale socket file from the previous boot may
# already exist but refuses connections). nix then either swallows the
# store connection error and falls through to a network fetch (DNS often
# still down too), or aborts with "opening a connection to remote store
# 'daemon' previously failed" - either way the one-liner fails and the
# caller's NIX_PATH becomes empty. The resolve sequence is now:
#
#   1. wait for the daemon to actually listen (only when nix uses it)
#   2. fresh eval; success refreshes the cache and prints the result
#   3. eval failed or empty: fall back to the last cached result
#   4. no cache: surface nix's own diagnostics and exit nonzero

# step 1: the stale socket file survives reboots, so watch for the
# listening state via ss instead of testing the path; skip entirely when
# nix does not run against the daemon, or when ss cannot answer - the -x
# flag is not universal (darwin's ss rejects it with rc 255), which would
# otherwise burn the whole wait for nothing
daemon_sock=/nix/var/nix/daemon-socket/socket
if command -v ss > /dev/null 2>&1 && ss -xln > /dev/null 2>&1 \
    && test "${NIX_REMOTE:-}" = daemon; then
  for _ in {1..50}; do
    ss -xln | grep -q "$daemon_sock" && break
    sleep 0.1
  done
fi

# step 2 setup: the cache is keyed by cksum of the requested params, so a
# different src maps to its own last-good entry instead of inheriting an
# unrelated one; err_file captures nix's stderr so steps 3 and 4 decide
# what reaches the terminal (the boot-time transient would only pollute
# it). It is mktemp-named because this script also runs as root in the
# first boot shell - a predictable name in /tmp could be pre-created as
# a symlink to redirect the write
cache_file="${XDG_CACHE_HOME:-$HOME/.cache}/niv-shim/last-nix-path.$(printf '%s' "$params" | cksum | cut -d ' ' -f 1)"
err_file="$(mktemp "${TMPDIR:-/tmp}/niv-shim-err.XXXXXX")" \
  || err_file="${TMPDIR:-/tmp}/niv-shim-err.$$"

# step 2: fresh resolve; the empty-result check matters because eval can
# exit 0 without producing a usable path, which must still count as
# failure so step 3 gets a chance
raw="$(nix eval --impure --expr "import ./nix-path-for-paths.nix ${params}" 2>"$err_file")"
rc=$?
if test "$rc" -eq 0; then
  out="$(printf '%s' "$raw" | xargs)"
  if test -n "$out"; then
    rm -f "$err_file"
    mkdir -p "${cache_file%/*}"
    printf '%s\n' "$out" > "$cache_file"
    printf '%s\n' "$out"
    exit 0
  fi
fi

# step 3: eval failed (or came back empty): reuse the last good result so
# the calling shell still gets a usable NIX_PATH; nix's own error stays
# out of the terminal - only this notice is shown
if test -s "$cache_file"; then
  rm -f "$err_file"
  printf 'nix-path-via-niv: nix eval failed, reusing cached NIX_PATH\n' >&2
  cat "$cache_file"
  exit 0
fi

# step 4: no cached fallback - surface nix's own diagnostics instead
cat "$err_file" >&2
rm -f "$err_file"
test "$rc" -ne 0 || rc=1
exit "$rc"
