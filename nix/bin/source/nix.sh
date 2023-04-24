# This file is expected to be sourced not executed

function safe_source() {
  test -f "${1}" && source "${1}"
}

# try standard linux way first
if ! command -v nix > /dev/null; then
  safe_source /etc/profile.d/nix.sh
  safe_source /etc/profile.d/user-shim-for-nix-path.sh
fi

# try another way next
if ! command -v nix > /dev/null; then
  safe_source ~/.nix-profile/etc/profile.d/nix.sh
fi

# try another way next
if ! command -v nix > /dev/null; then
  safe_source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi

if ! command -v nix > /dev/null; then
  echo 'Warning: `nix` is still not found'
fi
