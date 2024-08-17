{ ... }:

# https://github.com/Sabrina-Fox/WM2-Help

# try below when the audio from bluetooth headset is choppy
# - maybe just forget the device and reconnect might just fix
# - or maybe just reboot?
# - or connect it on the Windows first and come back to linux and try again?
# - or maybe low battery causes the issue, who knows
# - one of above worked for me
{
  imports = [
    <nixos-hardware/gpd/win-max-2>
    ./laptop.nix
  ];

  # "ignore" means it ignores sleeping I guess
  # using this as gpd-wm2 seems to be a good device to act as a server
  # read: https://discourse.nixos.org/t/prevent-laptop-from-suspending-when-lid-is-closed-if-on-ac/12630
  services.logind.lidSwitchExternalPower = "ignore";
  # the above requires reboot to take effect or restart logind I guess
  # in reality when the power is unplugged, the logind will process the lid closing as if it's just closed at that moment of the power is unplugged but will this be prevented if the device is docked (for long) via `lidSwitchDocked`? - I need to figure that out
  # can debug with `systemctl status logind.service` and `cat /etc/systemd/logind.conf`
  # read about it at https://www.freedesktop.org/software/systemd/man/latest/logind.conf.html
  # remains unanswered: is there a way to "wake up" again when power is back on

  services.fprintd.enable = true;
  # how to enroll: `sudo fprintd-enroll [username]`
  # how to verify: `fprintd-verify`
  # check from gnome setting: "Settings > Users"

  # regarding fprintd in general:
  # - https://sbulav.github.io/nix/nix-fingerprint-authentication/

  # INFO: [Troubleshoot] when device ever disappear (with `lsusb`)
  # - this can be caused by bios "favoring" Windows - so might as well disable on Windows side
  # - fixing is possible by resetting the bios would help https://github.com/Sabrina-Fox/WM2-Help?tab=readme-ov-file#potential-fix-for-touchscreen-or-other-hardwareeg-fingerprint-sensor-related-issues

  # regarding fprintd specifically for GPD Win Max 2:
  # - https://wiki.archlinux.org/title/GPD_Win_Max
  # - https://www.reddit.com/r/linuxmint/comments/1cc7miz/comment/l18o6kw/
  # - https://gitlab.freedesktop.org/libfprint/libfprint/-/issues/610#note_2407155
  # - https://github.com/ericlinagora/libfprint-CS9711

  # use this until it merges to the upstream according to the issue above
  nixpkgs.overlays = [
    (final: prev: {
      libfprint = prev.libfprint.overrideAttrs (oldAttrs: {
        version = "git";
        src = final.fetchFromGitHub {
          owner = "ericlinagora";
          repo = "libfprint-CS9711";
          rev = "c242a40fcc51aec5b57d877bdf3edfe8cb4883fd";
          sha256 = "sha256-WFq8sNitwhOOS3eO8V35EMs+FA73pbILRP0JoW/UR80=";
        };
        nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [
          final.opencv
          final.cmake
          final.doctest
        ];
      });
    })
  ];
}
