{ ... }:

# https://blog.cloudflare.com/virtual-networking-101-understanding-tap/
{
  services.cloudflare-warp.enable = true;

  # INFO: usages on connect/disconnect:
  # - need `warp-cli registration new` running once before `warp-cli connect`
  #   - don't believe the message `Success` though until you actually check with `warp-cli status`
  # - `warp-cli disconnect` to disable
  # - debug commands:
  #   - `warp-cli status`
  #   - `warp-cli settings`
  #   - `ip link` and `ip a` # not ip r
  #   - `ip tuntap`
  #   - `ls /sys/devices/virtual/net/CloudflareWARP`
  # - `warp-cli connect` can fail if tailscale is already up
  #   - how to check why it failed
  #     ```
  #     $ warp-cli connect
  #     Success
  #     # but this can be a lie as we can troubleshoot via `warp-cli`
  #
  #     $ warp-cli status
  #     Status update: Connecting
  #     # this is not the true result yet; wait a little and try again
  #
  #     $ warp-cli status
  #     Status update: Unable
  #     Reason: Connectivity check failed due to DNS Lookup Failed
  #     # this is the true reason - it says DNS lookup failed, we can assume that tailscale magic DNS interfered
  #     ```
  #   - run `sudo tailscale down && warp-cli connect && sleep 5 && sudo tailscale up` instead to mitigate the issue
  #
  # WARN: currently `warp-cli connect` will break the DNS (in a pure sense - which details will be mentioned later) with zerotier and tailscale and I haven't found a way to configure it to play nicely together
  # `warp-cli dns fallback list` will show that there are names will fallback;
  # however `warp-cli dns default-fallbacks` will show you that none of the IP adress for tailscale and zerotier will be included in the fallback list (as DNS IP address not as the root domain name) and that's why DNS for tailscale and zerotier get broken.
  # the network itself is reachable if you connect via IP address
  #
  # NOTE: however! only the `dig|nslookup|host` fail not the actual connection like browsers/ssh/curl/ping. What gives?
  # Turns out, it's thanks to the use of avahi via ./mdns.nix
  # my `/etc/nsswitch.conf` looks like below
  # ```
  # # ...
  # hosts:     mymachines mdns4_minimal [NOTFOUND=return] resolve [!UNAVAIL=return] files myhostname dns mdns4
  # # ...
  # ```
  # And we can see that 'mdns4_minimal' (via avahi) is in an earlier order than 'resolve' (via `/etc/resolv.conf`).
  # I already knew that most programs (browsers, ssh, curl, ping, etc.) makes use of `/etc/nsswitch.conf` so `*.local` can be resolved.
  # However, the thing that I didn't know that avahi is that it also resolves any other domains too not just `.local`
  # Which you can test via these commands:
  # - `avahi-resolve-host-name google.com [-v]`
  # - `avahi-resolve-host-name your-ts-host.your-ts-net-name.ts.net`
  # - `avahi-resolve-host-name your-zt-host.home.arpa`
  # That's awesome and this means most of DNS resolution was actually via avahi all this time!
  # But wait, how did it even know which IP address to use for each search domain though?
  # Does avahi somehow works similar to `systemd-resolved` that makes use of the configuration from `resolvectl status`?
  # It appears so as I tested by breaking the DNS via command below and run `avahi-resolve-host-name your-zt-host.home.arpa` again (which fails)!
  # `sudo resolvectl revert ztt6j5max4 # ztt6j5max is the interface my zerotier is using`
  # So thanks to avahi the broken DNS resolution (by `warp-cli connect`) is (mostly) not broken in practice!
  #
  # WARN: despite `avahi-resolve-host-name` works great with FQDN (by routing the DNS with search domain) it doesn't seem to do work with search domain;
  # meaning `avahi-resolve-host-name [your-ts-host|your-zt-host]` without FQDN would not work and it will be left for `/etc/resolv.conf`;
  # (which is broken with `warp-cli connect`.)
  #
  # INFO: so to sum up, in a practical sense FQDN DNS resolution is not broken but simply via a label without FQDN is broken as `warp-cli` takes over `systemd-resolved` by modifying `/etc/resolv.conf`
}
