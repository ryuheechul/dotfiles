{ pkgs, ... }:

# https://wiki.archlinux.org/title/Waydroid
{
  virtualisation.waydroid.enable = true;
  # `waydroid session stop` to stop waydroid
  # `sudo waydroid upgrade` to upgrade waydroid image (e.g. when it fails to start)
  # `sudo waydroid shell` to open the shell (e.g. when you want to `dmesg`)

  environment.systemPackages =
    let
      nur = import ../../pkgs/custom/nur.nix { pkgs = pkgs; };
    in
    [
      # https://wiki.archlinux.org/title/Waydroid#ARM_Apps_Incompatible
      # https://www.reddit.com/r/NixOS/comments/15k2jxc/need_help_with_activating_libhoudini_for_waydroid/
      nur.repos.ataraxiasjel.waydroid-script
      # above script is supposed to modify just `/var/lib/waydroid/overlay` according to https://github.com/casualsnek/waydroid_script/blob/1a2d3ad643206ad5f040e0155bb7ab86c0430365/stuff/general.py#L28
      # `waydroid-script` modifies `/var/lib/waydroid/overlay/` (on host) which I presume it's not possible within waydroid (since it will be ROM);
      # which explains why it has to run on host
      #
      # Install `libhoudini` arm translation (better for Intel?)
      # `sudo waydroid-script install libhoudini`
      # Install `libndk` arm translation (better for AMD?)
      # `sudo waydroid-script install libndk`
      # debug via `lf /var/lib/waydroid/overlay`
    ];

  # it looks like it randomly starts interfering the session sleep - https://github.com/waydroid/waydroid/issues/168
  # remedy: restart waydroid or upgrade the image to see if that fixes, or just manually stop waydroid session to prevent it for sure
}
