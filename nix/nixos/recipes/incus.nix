{ ... }:

# https://wiki.nixos.org/wiki/Incus
# https://hashnode.adityakumar.xyz/install-incus-on-nixos
let
  # Sticking with randomly generated one initially but declaring the static value explicitly
  # so automating the rest is easier.
  staticDNS = "10.130.211.1";
  cidr = "${staticDNS}/24";
  bridgeName = "incusbr0";
  core-https_address = ":34543";
  # Now we can either allow the port above with firewall or to serve with Tailscale.
  #
  ## In case to serve via Tailscale Service - https://console.tailscale.com/admin/services
  ## The approach below would fail to pass client cert.
  # `tailscale serve --service=svc:incus --https=443 https+insecure://localhost:34543`
  ## This way would work to pass client cert to Login with TLS
  # `tailscale serve --service=svc:incus --tcp=443 tcp://localhost:34543`
in
{
  virtualisation.incus = {
    enable = true;
    ui.enable = true;

    # These two below are optional to make incus discoverable without
    # running `incus webui` manually
    #
    # 1. Force Incus to start at boot and stay running (disables idling out)
    socketActivation = false;

    # Debug preseed failure with `systemctl status incus-preseed.service`
    # and read more about it here, https://search.nixos.org/options?channel=unstable&query=incus&type=options#show=option%253Avirtualisation.incus.preseed

    # 2. Configure Incus to serve the UI via its HTTPS API address
    preseed = {
      # Server-wide settings, https://linuxcontainers.org/incus/docs/main/server_config/
      config = {
        "core.https_address" = core-https_address; # Binds to port with core-https_address on all interfaces
      };

      # Declarative Network Zones (DNS forwarding)
      networks = [
        {
          name = bridgeName;
          type = "bridge";
          config = {
            # We define a static, predictable subnet here
            "ipv4.address" = cidr;
            # An alternative is "auto" but that would make it hard to make other things like DNS
            # configuration from host difficult.
            # "ipv4.address" = "auto";
            # Setting addresses to "auto" makes Incus dynamically pick a random
            # private IP pool range and handle DHCP allocations automatically.

            "ipv4.nat" = "true";

            # Enable ipv6 too since why not.
            "ipv6.address" = "auto";
            "ipv6.nat" = "true";
          };
        }
      ];

      # Define default storage pool even and it seems to be the same result
      # even without declaring this.
      storage_pools = [
        {
          name = "default";
          driver = "btrfs";
        }
      ];

      profiles = [
        # Define the default profile to link everything together
        # so that network and disk is always configured for a new instance.
        {
          name = "default";
          devices = {
            root = {
              path = "/";
              pool = "default";
              type = "disk";
            };
            eth0 = {
              name = "eth0";
              network = bridgeName;
              type = "nic";
            };
          };
        }
      ];
    };
  };

  # Declare the bridge unmanaged for NetworkManager just in case.
  networking.networkmanager.unmanaged = [ "interface-name:${bridgeName}" ];

  # This makes the *.incus resovlable from the host.
  systemd.network = {
    # Without enabling, the network below would not have any impact.
    enable = true;
    # `resovlectl` to verify below.
    networks."40-${bridgeName}" = {
      matchConfig.Name = bridgeName;
      networkConfig = {
        DNS = [ staticDNS ];
        Domains = [ "~incus" ];
      };
    };
    # Above may require `systemctl restart incus.service` due to transient conflict
    # on the bridge between networkd and incus.
  };

  # I will work on more on the other detailed options below as I actually gain the experience with Incus

  # https://scvalex.net/posts/54/
  # https://developers.redhat.com/blog/2020/08/18/iptables-the-two-variants-and-their-relationship-with-nftables#two_variants_of_the_iptables_command
  networking.nftables.enable = true;

  # https://wiki.nixos.org/wiki/Incus#Networking/Firewall
  # debug with `sudo nft list ruleset` and look at "chain input-allow"
  networking.firewall.interfaces.${bridgeName} = {
    allowedTCPPorts = [
      53
      67
    ];
    allowedUDPPorts = [
      53
      67
    ];
  };
  # or below as replacement of above
  # networking.firewall.trustedInterfaces = [ bridgeName ];

  security.apparmor.enable = true; # requires reboot

  # These are what make it possible for DNS query from external network to go through.
  #
  # # A. This unblocks the port so it can connect to `incusbr0`
  # networking.firewall = {
  #   allowedUDPPorts = [ 53 ];
  #   allowedTCPPorts = [ 53 ];
  # };
  #
  # # B. in addition to A. the host's systemd-resolved can act as a "bridge" name server.
  # services.resolved = {
  #   settings.Resolve = {
  #     DNSStubListener = "yes";
  #     DNSStubListenerExtra = "0.0.0.0";
  #   };
  # };
  #
  # But in case to handle this from a interface what's being blocked,
  # see firewall config from `./tailscale.nix`.
}
