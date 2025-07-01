# This file is expected to be sourced not executed

# see reference at https://nixos.org/manual/nix/stable/command-ref/conf-file.html

# making sure NIX_CONFIG exist especially `nix eval requires nix-command feature`
printenv NIX_CONFIG | grep nix-command >/dev/null ||
  export NIX_CONFIG='extra-experimental-features = nix-command flakes'
# also run `nix show-config` to see interpreted values

# if test "$(uname -s)" = "Darwin"; then
#   test "$(arch)" = "arm64" \
#     && export NIX_CONFIG="${NIX_CONFIG}
# extra-platforms = x86_64-darwin aarch64-darwin"
# fi
# turn off above as `nix show-config | grep platform` already shows the value below
# extra-platforms = x86_64-darwin
