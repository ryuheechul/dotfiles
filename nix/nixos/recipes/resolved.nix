{ ... }:

# this enables systemd-resolved - https://systemd.io/RESOLVED-VPNS/
{
  # the benefits and motivation is explained at https://tailscale.com/blog/sisyphean-dns-client-linux
  # this basically put a stop on the fight over `/etc/resolv.conf`
  services.resolved.enable = true;

  # debug with:
  # - `cat /etc/resolv.conf`
  # - `resolvectl status`
}
