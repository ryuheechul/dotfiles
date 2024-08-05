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
  # Actually I found a case that it didn't work with with the trick involving Waydroid but working with captive-browser! So I guess these two can complement each other.
}
