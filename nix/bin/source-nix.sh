# try standard linux way first
if [ -z "$(command -v nix)" ]; then
  . /etc/profile.d/nix.sh || true
  . /etc/profile.d/user-shim-for-nix-path.sh || true
fi

# try another way next
if [ -z "$(command -v nix)" ]; then
  . ~/.nix-profile/etc/profile.d/nix.sh || true
fi

if [ -z "$(command -v nix)" ]; then
  echo 'Warning: `nix` is still not found'
fi
