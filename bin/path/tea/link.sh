#!/usr/bin/env bash

# TODO: when the list grows, spin out the list to something like `packages.txt`
# edit here to add more files separate via space
direct_links="rust bun~0.4 ruby" # added ruby temporarily since graalvm's ruby (../../../nix/pkgs/lang/graalvm.nix) fails https://github.com/schasse/tmux-jump

script_d="$(dirname "$0")"
target_d="${script_d}/bin"

link-tea ()
{
  test -z "${_silence_log}" && echo "trying to link ${1}"

  if test "${1}" = rust; then
    echo 'group of binaries for rust detected and linking these ones instead:'
    # https://github.com/teaxyz/pantry.core/blob/main/projects/rust-lang.org/package.yml
    for p in cargo cargo-clippy cargo-fmt clippy-driver rust-analyzer rust-gdb rust-gdbgui rust-lldb rustc rustdoc rustfmt
    do
      echo "- ${p}"
      _silence_log=1 link-tea "${p}"
    done
    return
  fi

  which tea | xargs -I _ ln -sf _ "${target_d}/${1}"
}

indirect-link ()
{
  echo "aliasing ${1} to ${2}"
  ln -sf "${1}" "${target_d}/${2}"
}

# remove everything first for a clean start
rm "${target_d}/"*

for direct_link in ${direct_links}
do
  link-tea "${direct_link}"
done

# TODO: maybe separate data and procedures so the data can go up to the top
# indirect links
indirect-link bun~0.4 bun

"${script_d}/ls.sh"
