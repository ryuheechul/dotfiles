# bootstrap/foundation/nixos
This is highly experimental and not close to very usable yet

The main purpose here is to bootstrap NixOS guest inside UTM/QEMU on macOS host (Apple M1 device).

GPU Hardware acceleration somewhat works but many apps and video streaming glitches still (at least on M1).
- when choosing emulated display card as `virtio-ramfb-gl`

## Explain
- assumed that there is just one user
- `/etc/nixos/configuration.nix` is basically ignored (except to extract some information like timezone)
- `./hardware-configuration.nix` is a symlink to `/etc/nixos/hardware-configuration.nix`
- `./gen-files.sh` will generate `./hm.nix` and `./configuration.nix`
- `./base-configuration.nix` is what `./configuration.nix` is based on
- `./hm.nix` links to `../../../nix/home`
- `./switch.sh` will run `./gen-files.sh` and nixos-rebuild with them
