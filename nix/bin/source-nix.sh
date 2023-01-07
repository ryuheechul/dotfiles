# This file is expected to be sourced not executed

function safe-source() {
  test -f "${1}" && source "${1}"
}

# try standard linux way first
if test -z "$(command -v nix)"; then
  safe-source /etc/profile.d/nix.sh
  safe-source /etc/profile.d/user-shim-for-nix-path.sh
fi

# try another way next
if test -z "$(command -v nix)"; then
  safe-source ~/.nix-profile/etc/profile.d/nix.sh
fi

# try another way next
if test -z "$(command -v nix)"; then
  safe-source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi

if test -z "$(command -v nix)"; then
  echo 'Warning: `nix` is still not found'
fi
