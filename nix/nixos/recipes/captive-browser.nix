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
  # - type `detectpartal.firefox.com` for address bar
  #   - despite it's from Firefox it worked with Chrome too
  # - and go through regular process of captive portal
  # - make sure internet works on Waydroid
  # - now it should work on the host side as well!
  # - I would `waydroid session stop` to save resource
  #   - (unless you still need waydroid running even after)
}
