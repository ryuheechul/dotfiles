{ ... }:

{
  # https://nixos.wiki/wiki/Displaylink
  # first `nix-prefetch-url --name displaylink-580.zip https://www.synaptics.com/sites/default/files/exe_files/2023-08/DisplayLink%20USB%20Graphics%20Software%20for%20Ubuntu5.8-EXE.zip`
  services.xserver.videoDrivers = [ "displaylink" "modesetting" ];

  # I ventured out to this setting because my monitor started to not work after I changed the cable from HDMI to DisplayPort
  # and this setting actually didn't help the situation and what actually worked is following:
  # - (`cat .config/monitors.xml*` before delete if you want to see)
  # - `rm .config/monitors.xml*` via https://askubuntu.com/a/1450629/1666783
}
