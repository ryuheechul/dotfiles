#!/usr/bin/env bash

# to automate the answer just like using `yes` but with some "no"s
echo "nyy" | sh <(curl -L https://nixos.org/nix/install) --daemon
