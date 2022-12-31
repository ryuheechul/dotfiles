#!/usr/bin/env bash

## subscribe changes on macOS system appearance and update base16-shell accordingly

# discovered a good solution https://github.com/bouk/dark-mode-notify via https://www.stefanjudis.com/notes/a-terminal-script-to-get-notified-when-macos-changes-its-appearance-dark/

# alternative ways that I considered and dropped:
# - Shortcuts Automation - would have been an simple option except there is no such a thing yet for macOS although I don't find a way to manage the "code"
# - https://lgerckens.de/shortery/ - seems like a nice app but requires using Shortcuts on top of purchasing the app
# - python via https://github.com/ronaldoussoren/pyobjc - I'm not convinced to manage not just the script but also managing packages via virtualenv
# - AppleScript - is it possible to do such a thing? even if that's possible...

# Prior to this, I didn't know it was possible to run Swift code as if it was scripting lang via hashbang.
# Also not only Swift code is readable and concise but also it can access macOS API's natively.
# Thus, it became the most satisfying solution for me out of all the options that I explored for the purpose.

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
export UNSET_ALL_MY_ZSH_STUFF_LOADED=1
~/.dark-mode-notify/dark-mode-notify.swift zsh -lc 'source ~/.config/dfs-rhc/zshrc.d/zshrc; test "${DARKMODE}" = 1 && dark || light'
