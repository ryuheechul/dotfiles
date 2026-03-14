{ pkgs, config, ... }:

#########
#
# this recipe enables systemd-networkd and the motivation is to enhance the experience for using `systemd-nspawn` mainly:
# - configure network for the guest container (from the host side)
# - enable NAT for guest to be able to reach to the outer network
#
# number of options being enabled with the explanation of why they are needed
# if any doubt, simply disables each option to see what changes (breaks)
#
# `man systemd-nspawn` explains why `systemd-networkd` helps:
# ```
# Note that systemd-networkd.service(8) includes by default a network file
# /usr/lib/systemd/network/80-container-ve.network matching the host-side
# interfaces created this way, which contains settings to enable automatic
# address provisioning on the created virtual link via DHCP, as well as
# automatic IP routing onto the host's external network interfaces. It also
# contains /usr/lib/systemd/network/80-container-host0.network matching the
# container-side interface created this way, containing settings to enable
# client side address assignment via DHCP. In case systemd-networkd is running
# on both the host and inside the container, automatic IP communication from
# the container to the host is thus available, with further connectivity to
# the external network.
# ```
# INFO: in NixOS case, run `cat /run/current-system/sw/lib/systemd/network/80-container-ve.network` to see the file
#
#########

# https://nixos.wiki/wiki/Systemd-networkd
# checkout `./resolved.nix` as well
{
  environment.systemPackages = [ pkgs.mkosi ];

  systemd.network.enable = true;

  # https://search.nixos.org/options?show=systemd.network.wait-online.enable
  systemd.network.wait-online.enable = false;

  # https://nixos.org/manual/nixos/stable/index.html#sec-container-networking
  networking.networkmanager.unmanaged = [ "interface-name:ve-*" ]; # doesn't seem to be really necessary but whatever

  networking.nftables.enable = true; # it works either way

  # https://nixos.wiki/wiki/Networking
  # https://discourse.nixos.org/t/nat-and-port-forwarding/28929/2
  networking.nat = {
    # it's basically doing this https://wiki.archlinux.org/title/Internet_sharing#Enable_NAT
    enable = true;

    # https://github.com/NixOS/nixpkgs/issues/72580#issuecomment-1783933476
    internalInterfaces = if config.networking.nftables.enable then [ "ve-*" ] else [ "ve-+" ];
    # debug with:
    # - run `[sudo] iptables --list -v -t nat|less` to debug with `iptables`
    # - or `[sudo] nft list ruleset ip |less` to debug with `nft[able]`

    # externalInterface = "wlp0s20f3"; # I didn't need to specify! Maybe thanks to some smart figuring out things going on?
  };
}

# one network issue that I couldn't solve with this recipe (or module) is;
# the guest container not having the default route setup
# e.g. `ip route` gives you the below
# ```
# 169.254.0.0/16 dev host0 proto kernel scope link src 169.254.107.27 metric 2048
# ```
# which I can fix with;
# `ip route add default via 169.254.47.216 src 169.254.107.27`
#                                  |                  |
#                  host side IP at `ve-xx..`   guest container IP
#
# which will give you something like this below
# ```
# default via 169.254.47.216 dev host0 src 169.254.107.27
# 169.254.0.0/16 dev host0 proto kernel scope link src 169.254.107.27 metric 2048
# ```
# and with all the host side configuration from the code in this file, the guest should be able to reach the outer network!
# no wonder why docker succeeded in terms of UX and everything (even today (2024) figuring all this out for systemd-nspawn was not easy...)
# but it was fun in the end!
#
# just expand this commentary documentation to be more complete, I will leave how I spawned the guest containers to test all these
#
# ```
# # this will pull the image and extract at /var/lib/machines
# importctl pull-tar -mN https://cloud-images.ubuntu.com/jammy/current/jammy-server-cloudimg-amd64-root.tar.xz jammy
# machinectl start jammy # it uses `systemd-nspawn` to boot
# machinectl shell jammy # wait until complete the boot or check with `journalctl -u systemd-nspawn@jammy.service` if you are impatient/curious
# # don't forget to set up the default route as explained somewhere above
# ```
#
# and doing the same thing with another image
# ```
# importctl pull-tar -mN https://hub.nspawn.org/storage/debian/bookworm/tar/image.tar.xz bookworm --verify=no # yeah you should verify...
# machinectl start bookworm
# machinectl shell bookworm
# ```
#
# by the way, running two containers at the same time seem to conflict;
# with their link-local (169.254.0.0/16) IP ranges as both `ve-xxx` links use that range (especially at the host)
#
# oh btw, DNS is not working yet, I guess that's my next issue to resolve, lol
#
# again I understand why docker won (the hearts)...
#
# probably a lot to do with potential poor defaults that can be viewed by `systemctl cat systemd-nspawn@.service`
