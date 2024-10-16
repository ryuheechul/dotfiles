# - copy this file to `./mix-and-match.nix` and tailor to the local device
# - only leave the ones that you actually intended to use and comment out ones that you don't

{ ... }:

{
  imports = [
    # hardware/device specifics
    ./recipes/firmware.nix
    # ./recipes/laptop.nix - this usually gets included via others like surface-pro... and gpd-win-max-2
    ./recipes/surface-pro-intel.nix
    ./recipes/surface-pro-9-intel.nix
    ./recipes/gpd-win-max-2.nix
    ./recipes/no-sleep.nix
    # handling oom to prevent freezing
    ./recipes/oom.nix
    ./recipes/compat.nix
    ./recipes/perf-tweaks.nix
    # when it's virtualised/containerized
    ./recipes/vm-guest-utm.nix
    ./recipes/vm-guest-utm-mount.nix
    ./recipes/wsl.nix
    ./recipes/mtu-probing.nix
    # services
    ./recipes/desktop-gnome.nix
    ./recipes/keyd-host-windows.nix
    ./recipes/keyd-guest.nix
    ./recipes/flatpak.nix
    ./recipes/openssh.nix
    ./recipes/pam-sshagent.nix
    ./recipes/mdns.nix
    ./recipes/airplay.nix
    ./recipes/steam.nix
    ./recipes/rdp-server.nix
    ./recipes/docker.nix
    ./recipes/virtualization.nix
    ./recipes/waydroid.nix
    ./recipes/rosetta.nix
    ./recipes/captive-browser.nix
    ./recipes/tailscale.nix
    ./recipes/1password.nix
    ./recipes/dropbox.nix
    # not proven
    # ./recipes/displaylink.nix
    # ./recipes/syncthing.nix - should not be used to favor ../home/services/syncthing.nix except for testing
  ];
}
