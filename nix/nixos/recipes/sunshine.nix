{ ... }:

# https://wiki.nixos.org/wiki/Sunshine
{
  services.sunshine = {
    enable = true;
    autoStart = true;
    capSysAdmin = true; # only needed for Wayland -- omit this when using with Xorg
    openFirewall = true;
  };
}

# my use case for sunshine might be unusual;
# I use this (and moonlight) to augment the experience of remote desktop not really for the gaming
# so what features the sunshine/moonlight unlocks for me?
# - basic remote desktop (easily activatable via SSH)
#   - which can turn on Gnome Remote Desktop
#     - `grdctl status` to verify or even setup but via GUI is easier
#     - more info regarding above is written at ./rdp-server.nix
# - complement RDP experience with audio since sunshine/moonlight redirect audio
#   - RDP can do the audio but the experience was not good
#     - (choppy on client and remote still produced the audio on its own speaker which is not what I want)
#       - (I'm probably unaware of how to do this properly though)
#   - if audio is the main and screen is through other medium (e.g. RDP) then reduce the resolution to save compute resource and bandwidth
#   - why not just use sunshine/moonlight over RDP though?
#     - apparently RDP can be more appropriate for "regular" usage (e.g. clipboard sync - https://github.com/moonlight-stream/moonlight-qt/issues/1103)
#
# situation 1 - both RDP and sunshine/moonlight can't be initiated when the remote (gnome) session is idle
# - in my case the it's a laptop that the lid is closed and keyboard is not accessible (when lid is closed)
# - SSH and `loginctl` to rescue!
#   - SSH to the machine and `loginctl` to find out the session number (that is idle)
#   - and run `loginctl unlock-session [session-id]`
#   - verify the session is no longer idle with `loginctl`
#   - and try again with moonlight or RDP connection and it should work
