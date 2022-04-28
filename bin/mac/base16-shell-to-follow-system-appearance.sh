#!/usr/bin/env bash

## clone dark-mode-notify

# TODO: maybe find better location to manage this
test -d ~/.dark-mode-notify \
	|| git clone https://github.com/bouk/dark-mode-notify ~/.dark-mode-notify

## install swift to avoid swift binary being older than sdk - like an error like below
# ```
# /Library/Developer/CommandLineTools/SDKs/MacOSX12.3.sdk/usr/lib/swift/_Concurrency.swiftmodule/x86_64-apple-macos.swiftinterface:5:8: error: failed to build module 'Swift'; this SDK is not supported by the compiler (the SDK is built with 'Apple Swift version 5.6 (swiftlang-5.6.0.323.32 clang-1316.0.20.8)', while this compiler is 'Apple Swift version 5.5 (swiftlang-1300.0.27.6 clang-1300.0.27.2)'). Please select a toolchain which matches the SDK.
# ```
brew bundle --file=- <<-EOF
brew "swift"
EOF

## export swift bin path
# for dark-mode-notify.swift to be able to pick up the right swift binary
export PATH="$(brew --prefix swift)/bin:${PATH}"

## run dark-mode-notify.swift to fix base16-shell based on system's theme
# `dark` and `light` are my custom zsh functions so there are some stuff to load the functions like exporting variable and source my ~/.zshrc.
export FORCE_LOAD_MY_ZSH_STUFF=1
~/.dark-mode-notify/dark-mode-notify.swift zsh -lc 'source ~/.zshrc; test "${DARKMODE}" = 1 && dark || light'
