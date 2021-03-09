#!/usr/bin/env sh

cat << EOF > /etc/profile.d/user-shim-for-nix-path.sh
# the filename is intentionally behind `nix.sh`
export NIX_PATH=\${HOME}/.nix-defexpr/channels:\${NIX_PATH}
EOF
