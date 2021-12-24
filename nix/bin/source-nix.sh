# try standard linux way first
if [ -z "$(command -v nix)" ]; then
  [ -f /etc/profile.d/nix.sh ] && \
    . /etc/profile.d/nix.sh
  [ -f /etc/profile.d/user-shim-for-nix-path.sh ] && \
    . /etc/profile.d/user-shim-for-nix-path.sh
fi

# try another way next
if [ -z "$(command -v nix)" ]; then
  [ -f ~/.nix-profile/etc/profile.d/nix.sh ] && \
    . ~/.nix-profile/etc/profile.d/nix.sh
fi

# try another way next
if [ -z "$(command -v nix)" ]; then
  [ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ] && \
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
fi

if [ -z "$(command -v nix)" ]; then
  echo 'Warning: `nix` is still not found'
fi
