# bootstrap/foundation/nixos

This is highly experimental and not even close to very usable (especially with
GUI apps) yet.

The main purpose here are:

- to bootstrap NixOS guest inside UTM/QEMU on macOS host (Apple M1 device)
- gain a little bit of (artificial) experiences of using NixOS just for fun.

## Things That I Haven't Figured Out

_I'm choosing emulated display card as `virtio-ramfb-gl` for the hardware
acceleration with QEMU._

GPU Hardware acceleration somewhat works but many apps and video streaming still
glitch a lot (with my current usage as VM on M1 hardware).

I believe this is where
[Asahi Linux](https://asahilinux.org/2022/12/gpu-drivers-now-in-asahi-linux/) is
pioneering and not all the work is backported to the upstream yet. And since I
couldn't find any other (easy) way to mitigate this, I'm just waiting for
improvements (hopefully) merged to the kernel later.

List of desktop/apps that didn't work:

- alacritty
- kitty
- chrome (generally works but with lots of glitches)
- firefox
- xfce desktop
- plasma desktop

## Explain

- assumed `/etc/nixos/configuration.nix` is good default to start with
- real meat is at `../../../nix/nixos/`
- `./gen-configuration.sh` will generate `./hm.nix` and `./configuration.nix` to glue
  everything together
- `./hm.nix` basically links to `../../../nix/home`
- `./switch.sh` will run `./gen-configuration.sh` and run `nixos-rebuild` with the
  generated files

## Resources

- https://nixos.wiki/wiki/NixOS_as_a_desktop
- https://github.com/mikeroyal/NixOS-Guide
