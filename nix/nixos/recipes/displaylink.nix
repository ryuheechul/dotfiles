{ ... }:

# [DisplayLink](https://en.wikipedia.org/wiki/DisplayLink) is a specific technology
{
  # https://nixos.wiki/wiki/Displaylink
  # first `nix-prefetch-url --name displaylink-580.zip https://www.synaptics.com/sites/default/files/exe_files/2023-08/DisplayLink%20USB%20Graphics%20Software%20for%20Ubuntu5.8-EXE.zip`
  services.xserver.videoDrivers = [ "displaylink" "modesetting" ];
}

# Initially I ventured out to try setting because my monitor started to not work after I changed the cable from HDMI to DisplayPort
# and this setting actually didn't help (because DisplayLink is a specific technology nothing to do with "regular" display setups) and what actually worked is following:
# - (`cat ~/.config/monitors.xml*` before delete if you want to see)
# - `rm ~/.config/monitors.xml*` via https://askubuntu.com/a/1450629/1666783
# if above doesn't work, try to adjust the refresh rate especially if there are lots of option for certain display
# find out about resolution and refresh rate with `xrandr -q --verbose`
#
# also when screen flickers, try to see if it's a hardware issue by trying different combinations of different cables (in my case even with same speed spec cables, one worked well when the other not)
