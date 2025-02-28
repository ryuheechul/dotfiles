{ ... }:

{
  # captive-browser # Dedicated Chrome instance to log into captive portals without messing with DNS settings
  programs.captive-browser.enable = true;
  programs.captive-browser.interface = "wlp0s20f3";

  # Unfortunately `captive-browser` on Nix doesn't seem to be working well nor actively being maintained.
  #
  # However I found another trick! (that doesn't involve with `captive-browser`)
  # - make sure you set up Waydroid in my case it should be enabled by ./waydroid.nix
  #   - technically it doesn't have to Waydroid but any subsystem
  #     - (that can handle captive portal better than regular linux)
  # - open a browser (I used Chrome) from Waydroid
  # - type `detectportal.firefox.com` for address bar
  #   - despite it's from Firefox it worked with Chrome too
  # - and go through regular process of captive portal
  # - make sure internet works on Waydroid
  # - now it should work on the host side as well!
  # - I would `waydroid session stop` to save resource
  #   - (unless you still need waydroid running even after)
  # - if there is any issue, few trouble shootings you can do:
  #   - forget the network and reconnect freshly and try again
  #   - if `detectportal.firefox.com` doesn't resolve the domain name,
  #     use your other device to find the ip for the domain and type that to the address bar instead
  #   - you may need to turn off VPN like Tailscale that changes the `nameserver` at `/etc/resolv.conf`
  #     until captive the issue is resolved
  #     - there is issue regarding above: https://github.com/tailscale/tailscale/issues/1634
  #     - in case tailscale's detection (https://tailscale.com/kb/1457/captive-portals) isn't helping
  #       - [v1.72 or higher should have better support and communication on captive portal](https://tailscale.com/changelog#2024-08-19)
  # Actually I found a case that it didn't work with with the trick involving Waydroid but working with captive-browser! So I guess these two can complement each other.
  #
  # And when nothing seems to work, we might need to turn off `resolved` temporarily by the line below
  # `services.resolved.enable = lib.mkForce false;` # to temporarily deal with not so up-to-date captive portals
  # make sure to undo this after captive portal step is done and if somehow DNS fails sometime after this you may want to run `systemctl restart systemd-resolved.service`
  #
  # More on captive portal
  # - https://www.chromium.org/chromium-os/chromiumos-design-docs/network-portal-detection/
  # - https://datatracker.ietf.org/doc/html/rfc8952
  #
  # Alternatives to `detectportal.firefox.com`
  # - http://neverssl.com
  # - http://captive.apple.com
  # - http://connectivitycheck.gstatic.com/generate_204
  # - http://1.1.1.1 (when DNS is unstable)
}
