{ lib, config, ... }:

# this enables systemd-resolved - https://systemd.io/RESOLVED-VPNS/
# but beware... https://www.reddit.com/r/linux/comments/18kh1r5/im_shocked_that_almost_no_one_is_talking_about/
{
  # the benefits and motivation is explained at https://tailscale.com/blog/sisyphean-dns-client-linux
  # this basically put a stop on the fight over `/etc/resolv.conf`
  services.resolved.enable = true;
  services.resolved.llmnr = "false";
  services.resolved.settings = {
    Resolve = lib.mkIf config.services.avahi.enable {
      # maybe I should disable MulticastDNS regardless of using avahi or not?
      MulticastDNS = "false";
      # anyhow now host/dig/nslookup will stop resolving .local when ping does - https://serverfault.com/a/579996/633069
    };
  };

  # debug with:
  # - `cat /etc/resolv.conf`
  # - `resolvectl status`
}
