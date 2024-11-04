{ ... }:

# https://wiki.nixos.org/wiki/Incus
# https://hashnode.adityakumar.xyz/install-incus-on-nixos
{
  virtualisation.incus = {
    enable = true;
    # `incus admin init` to get started but couldn't pass the step with the error below actually...
    # 'Error: Failed to create local member network "incusbr0" in project "default": The DNS and DHCP service exited prematurely: exit status 127 ("dnsmasq: error while loading shared libraries: libdbus-1.so.3: cannot open shared object file: No such file or directory")'
    ui.enable = true;
  };

  # I will work on more on the other detailed options below as I actually gain the experience with Incus

  # https://scvalex.net/posts/54/
  # https://developers.redhat.com/blog/2020/08/18/iptables-the-two-variants-and-their-relationship-with-nftables#two_variants_of_the_iptables_command
  networking.nftables.enable = true;

  # https://wiki.nixos.org/wiki/Incus#Networking/Firewall
  networking.firewall.interfaces.incusbr0 = {
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
  # networking.firewall.trustedInterfaces = [ "incusbr0" ];

  security.apparmor.enable = true; # requires reboot
}
