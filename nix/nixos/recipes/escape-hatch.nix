{ config, ... }:

# this is more of an example than a "recipe" that should be imported as-is

let
  rev = "master"; # or "a3c0b3b21515f74fd2665903d4ce6bc4dc81c77c", etc.
  allowUnfree = config.nixpkgs.config.allowUnfree;
  pinnedPkgs = import ../../pkgs/custom/pkgs.nix { rev = rev; allowUnfree = allowUnfree; };
in
{
  # examples of drifting the versions of certain packages on purpose from the `nixpkgs` as an escape hatch
  services.pipewire.wireplumber.package = pinnedPkgs.wireplumber;
  services.pipewire.package = pinnedPkgs.pipewire;
  hardware.bluetooth.package = pinnedPkgs.bluez;
  programs._1password-gui.package = pinnedPkgs._1password-gui;
  services.tailscale.package = pinnedPkgs.tailscale;
}
