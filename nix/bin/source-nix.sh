# This file is expected to be sourced not executed

# try standard linux way first
if test -z "$(command -v nix)"; then
  test -f /etc/profile.d/nix.sh && \
    . /etc/profile.d/nix.sh
  test -f /etc/profile.d/user-shim-for-nix-path.sh && \
    . /etc/profile.d/user-shim-for-nix-path.sh
fi

# try another way next
if test -z "$(command -v nix)"; then
  test -f ~/.nix-profile/etc/profile.d/nix.sh && \
    . ~/.nix-profile/etc/profile.d/nix.sh
fi

# try another way next
if test -z "$(command -v nix)"; then
  test -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh && \
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi

if test -z "$(command -v nix)"; then
  echo 'Warning: `nix` is still not found'
fi
