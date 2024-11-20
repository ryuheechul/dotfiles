{ ... }:

# https://wiki.nixos.org/wiki/Droidcam
# https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/programs/droidcam.nix
{
  # requires a reboot after due to loading "v4l2loopback" kernel module;
  # and it may rebuild a whole linux kernel
  programs.droidcam.enable = true;
}
