#!/usr/bin/env bash

## Some background information regarding this script
##
## # What it does
## - kind of fulling the intent of https://github.com/zerotier/zerotier-systemd-manager
##   - which is making sure to properly configure zerotier DNS experience in the client side
##     - if you are looking for a server side information, see ../zeronsd.nix and https://github.com/zerotier/zeronsd
## - `zerotier-systemd-manager` didn't work for me because it has the assumption of `networkd` being used
##   - however I opted in to using `systemd-resolved` which requires a different approach
##     - if you don't want to opt in for `systemd-resolved` than just rely on ../zerotier-dns-client.nix
##       - although it's not reliable as well (read more on that from the file itself)
##
## # How it works
## - retrieve dns related information (that are going to be used with `resolvectl`) via `zerotier-cli`
## - parse it with `jq`
## - configure the "DNS routing" as suggested from https://systemd.io/RESOLVED-VPNS/ via `resolvectl` with the parsed values
##   - `systemd-resolve` command can actually do the same job but `resolvectl` has simpler CLI to do so
##
## # Why this way?
## - `allowDNS=1` of zerotier seems to be fooling for linux users for 4+ years...
##   - https://www.reddit.com/r/zerotier/comments/lmei2w/comment/gnw645q/
##   - and some workarounds descried at https://docs.zerotier.com/dns/#zerotier-systemd-manager doesn't seem to be reliable and portable
## - I'm used to how tailscale handles DNS (https://tailscale.com/kb/1235/resolv-conf) via their Magic DNS experience
##   - which was working fine even without `systemd-resolved` that I just discovered
##   - and it works even better without having to fight with /etc/resolv.conf
##     - read https://tailscale.com/blog/sisyphean-dns-client-linux for more about above
##     - (with systemd-resolve environment) you can see the different of `tailscale0` interface via `resolvectl` between the "up" and "down" state
##       - via `tailscale up` and `tailscale down`
##     - and this way works very well without intruding other interfaces, hence I desire the same result for the zerotier as well
##
## # Gotchas
## - you can't use without systemd-resolved, so you have to opt-in
## - you will need to manually run this script as often as this configuration is polluted by other force
##   - which I'm not sure what other application will mess with a specific zerotier interface but a possiblity
##   - this configuration should stay the same until there is a major changes flushes this configuration
##     - e.g. deletion of the zerotier network interface
##     - unfortunately rebooting wipes out the changes
##       - so currently it's required to run this script again with every reboot
##         - let me find a better solution for this...
## - (while it's not related to this script itself at all but) changing network (e.g. changing WiFi SSID) would cause zerotier to fail to connect
##   - (test that via `ping [zero dns ip]`)
##   - but this recovers after some arbitrarily (long) time
##   - note that the changing network (path) breaking zerotier is nothing to do with systemd-resolved configuration at all
##     - meaning that will happen regardless as it's the limitation of zerotier itself at the time of writing
##     - but with this DNS configuration in effect it might break DNS lookup for regular FQDN
##       - although it shouldn't since it shouldn't even route to this (maybe a bug from systemd-resolved)

# # Dependencies of this script:
# - bash
# - zerotier-cli
# - resolvectl (that is configured with systemd-resolved)
# - jq
#
# # How to run:
# simply run `./zerotier-dns-client.sh` and it should do the job
#
# # How to test
# lookup domain name via single label domain that would match your search domain
# e.g. `host mydevice # or "nslookup" or "dig +search" instead of "host"` and resolved in mydevice.home.arpa with an IP address
#      (assuming you have `mydevice` connected to the zerotier network and using `home.arpa` as domain)
#
# # How to undo
# `sudo resolvectl revert [interface]`

echo '[info] going to run `sudo zerotier-cli listnetworks -j` to parse necessary network information'
networks=$(sudo zerotier-cli listnetworks -j)

if ! echo "${networks}" | jq > /dev/null 2>&1; then
  echo "[error] the result below doesn't seem to be in the right format"
  echo "${networks}"
  exit 1
fi

# thanks to https://stackoverflow.com/a/33952539/1570165
echo $networks | jq -c '.[]' | while read network; do
  domain="$(echo $network | jq -r '.dns.domain')"
  dns_list="$(echo $network | jq -r '.dns.servers | join(" ")')"
  interface="$(echo $network | jq -r '.portDeviceName')"

  echo "[info] detected values: \$domain ($domain), \$dns_list ($dns_list), \$interface ($interface)"
  if test -z "$domain" || test -z "$dns_list" || test -z "$interface"; then
    echo "[warn] Any of the values above might be wrong so skpping this interface."
  else
    echo "[info] before the change:"
    resolvectl status $interface
    echo "[info] configuring with ..."
    set -x
    sudo resolvectl revert $interface
    sudo resolvectl dns $interface $dns_list
    sudo resolvectl default-route $interface "false"
    sudo resolvectl domain $interface $domain
    { set +x; } 2>/dev/null # thanks to https://stackoverflow.com/a/19226038/1570165
    echo "[info] after change:"
    set -x
    resolvectl status $interface
    { set +x; } 2>/dev/null
  fi
done

# TODO: (potential)
# - register this as a systemd service so that there is no need to manually run this
#   - however that means, some sort of continuously detecting the state of the environment necessary to function reliably
# - package this as a standalone program that doesn't require implicit dependencies like jq
#   - which allows this to be portable for any linux system
