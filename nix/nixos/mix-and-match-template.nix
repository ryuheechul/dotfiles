# copy this file to ./mix-and-match.nix and tailor to the local device
# aka "I wouldn't use this as is"

{ ... }:

{
  imports = [
    # hardware specifics
    ./recipes/surface-pro-intel.nix
    # handling oom to prevent freezing
    ./recipes/oom.nix
    # when it's virtualised
    ./recipes/vm-guest-utm.nix
    ./recipes/wsl.nix
    ./recipes/mtu-probing.nix
    # services
    ./recipes/openssh.nix
    ./recipes/pam-sshagent.nix
    ./recipes/mdns.nix
    ./recipes/airplay.nix
    ./recipes/desktop-gnome.nix
    ./recipes/docker.nix
    ./recipes/rosetta.nix
    ./recipes/waydroid.nix
    ./recipes/captive-browser.nix
    ./recipes/virtualization.nix
    ./recipes/flatpak.nix
    ./recipes/tailscale.nix
    ./recipes/1password.nix
    ./recipes/keyd-host-windows.nix
    ./recipes/keyd-guest.nix
    # not proven
    ./recipes/displaylink.nix
  ];
}
