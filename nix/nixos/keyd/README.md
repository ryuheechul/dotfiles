# What Are the Files Here for
They have the one same goal, mimic the macOS keybindings when makes sense.

But you can pick and choose the one makes sense the most depends on where is NixOS installed.

For example, if it's installed as VM on a macOS device, [./settings-min.nix](./settings-min.nix) would do.
If it's installed on a laptop originally for Windows, [./settings-full-windows.nix](./settings-full-windows.nix) might do.

List available keys with `nix-shell -p keyd --command 'keyd list-keys'`.
