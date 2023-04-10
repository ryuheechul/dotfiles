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

- assumed that there is just one user
- `/etc/nixos/configuration.nix` is basically ignored (except to extract some
  information like timezone)
- `./hardware-configuration.nix` is a symlink to
  `/etc/nixos/hardware-configuration.nix`
- `./gen-files.sh` will generate `./hm.nix` and `./configuration.nix`
- `./gen-local-pkgs.sh` will generate `./local-pkgs.nix`
- `./base-configuration.nix` is what `./configuration.nix` is based on
- `./hm.nix` links to `../../../nix/home`
- `./switch.sh` will run `./gen-files.sh` and nixos-rebuild with them

## Resources

- https://nixos.wiki/wiki/NixOS_as_a_desktop
