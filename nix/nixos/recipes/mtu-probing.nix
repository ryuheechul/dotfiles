{ ... }:

{
  # you may want to use this if it's [e.g. virtualized](./vm-guest-utm.nix)
  # and if there is a potential of MTU set low (lower than 1500) at host using VPN

  # articles regarding issues related MTU aka "ICMP black hole"
  # - https://github.com/moby/moby/issues/22297
  # - https://www.civo.com/learn/fixing-networking-for-docker
  # - https://sylwit.medium.com/how-we-spent-a-full-day-figuring-out-a-mtu-issue-with-docker-4d81fdfe2caf
  # - https://blog.cloudflare.com/path-mtu-discovery-in-practice
  # - https://news.ycombinator.com/item?id=39381508
  # - https://www.reddit.com/r/networking/comments/52mzzd/netipv4tcp_mtu_probing_how_does_it_work
  # - https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/7/html/kernel_administration_guide/working_with_sysctl_and_kernel_tunables
  # - https://packetlife.net/blog/2008/aug/18/path-mtu-discovery/
  # - https://www.networkworld.com/article/750618/opensource-subnet-best-networking-tweaks-for-linux.html

  # or echo 1 > /proc/sys/net/ipv4/tcp_mtu_probing
  # sysctl -w net.ipv4.tcp_mtu_probing=1
  boot.kernel.sysctl."net.ipv4.tcp_mtu_probing" = 1;
  # debug with
  # or cat /proc/sys/net/ipv4/tcp_mtu_probing
  # sysctl net.ipv4.tcp_mtu_probing

  # quick path mtu discovery
  # traceroute -F --mtu example.com

  # debug with macOS host side with `netstat -lnI utun3` if NixOS is running as guest VM
  # Name           Mtu   Network                                 Address                                    Ipkts Ierrs    Opkts Oerrs  Coll
  # utun3          1400  <Link#18>                                                                        9661516     0  2433007     0     0
  # utun3          1400  ...                                                                              9661516     -  2433007     -     -

  # enabling `tcp_mtu_probing` should be better than alternative below
  # networking.interfaces.enp0s1.mtu = 1400; # debug with `netstat -i` or `ip link`
}
