#!/usr/bin/env bash

git clone https://github.com/Chronial/wsl-sudo.git ~/.wsl-sudo

echo 'alias wudo="python3 ~/.wsl-sudo/wsl-sudo.py"' >> ~/.zshrc
