# What Are the Files Here for
They have the one same goal, mimic the macOS keybindings when makes sense.

But you can pick and choose the one makes sense the most depends on where is NixOS installed.

For example:

- as a VM (most key mappings are already done on the host side):
  - on any host (e.g. macOS), [./settings-min.nix](./settings-min.nix) would do
- as the host OS (no key mappings are done already):
  - [./settings-full.nix](./settings-full.nix) does the full mapping; pass `hardware = "pc"` or `hardware = "mac"` depending on the physical keyboard (as `../recipes/keyd-hardware-pc.nix` and `../recipes/keyd-hardware-mac.nix` do)
  - on a Mac, Command and Option are already flipped compared to a PC keyboard
  - the hardware-specific bindings live in [./modifier-keys.nix](./modifier-keys.nix)

List available keys with `nix-shell -p keyd --command 'keyd list-keys'`.
