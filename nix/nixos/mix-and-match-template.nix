# - copy this file to `./mix-and-match.nix` and tailor to the local device
# - only leave the ones that you actually intended to use and comment out ones that you don't

# - to accept the username from ../../bootstrap/foundation/nixos/configuration.nix
username:

# - from here on it's just like any other NixOS module
{ ... }:

{
  imports =
    [
      # hardware/device specifics
      ./recipes/firmware.nix
      ./recipes/droidcam.nix
      (import ./recipes/meta/tpm.nix username)
      ./recipes/auto-timezone.nix
      # virtualisations
      ./recipes/docker.nix
      # ./recipes/docker-rootful.nix
      # ./recipes/podman-rootful-docker.nix
      ./recipes/podman.nix
      ./recipes/libvirt.nix
      ./recipes/quickemu.nix
      ./recipes/incus.nix
      ./recipes/waydroid.nix
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
      ./recipes/vm-guest-qemu.nix
      ./recipes/vm-guest-utm.nix
      ./recipes/vm-guest-utm-mount.nix
      ./recipes/wsl.nix
      ./recipes/mtu-probing.nix
      # desktop
      ./recipes/desktop-gnome.nix
      ./recipes/desktop-cosmic.nix
      # ./recipes/desktop-kde.nix
      ./recipes/rdp-server.nix
      ./recipes/sunshine.nix
      # services
      ./recipes/resolved.nix
      ./recipes/keyd-host-windows.nix
      ./recipes/keyd-guest.nix
      ./recipes/flatpak.nix
      ./recipes/openssh.nix
      ./recipes/pam-sshagent.nix
      # ./recipes/no-tty-tickets.nix
      ./recipes/mdns.nix
      ./recipes/airplay.nix
      ./recipes/steam.nix
      ./recipes/rosetta.nix
      ./recipes/captive-browser.nix
      ./recipes/tailscale.nix
      ./recipes/cloudflare-warp.nix
      ./recipes/1password.nix
      ./recipes/dropbox.nix
      # not proven
      # ./recipes/displaylink.nix
      # ./recipes/syncthing.nix - should not be used to favor ../home/services/syncthing.nix except for testing
    ]
    # an example usage for `./containers/meta/ollama-amd.nix`
    # ++ (
    #   let
    #     tailnetName = "my-ts-net-name"; # excluding "ts.net" at the end
    #   in
    #   [
    #     (import ./containers/meta/ollama-amd.nix { tailnetName = tailnetName; })
    #   ]
    # )
    #
    # usage examples for `./recipes/meta` (you will need to replace values accordingly):
    # ++ (
    #   let
    #     network = "5y3o4u9r4n9e2t58"; # a Zerotier network
    #     address = "10.149.19.239"; # IP address of zeronsd
    #   in
    #   [
    #     # for Zerotier to work
    #     (import ./recipes/meta/zerotier.nix { network = network; })
    #     # for zeronsd to work (server only)
    #     (import ./recipes/meta/zeronsd.nix { network = network; })
    #     # for zeronsd to work (client only) - typically you should choose just either server or client
    #     (import ./recipes/meta/zerotier-dns-client.nix { address = address; })
    #   ]
    # )
    ++ [
      # stub line for to make it easy to toggle comment block above
    ];
}
