{ pkgs, ... }:

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

  # probably better to benefit from the latest kernel unless there is an issue
  boot.kernelPackages = pkgs.linuxPackages_latest;

  services.fprintd.enable = true;
  # how to enroll: `sudo fprintd-enroll [username]`
  # how to verify: `fprintd-verify`
  # check from gnome setting: "Settings > Users"

  # regarding fprintd in general:
  # - https://sbulav.github.io/nix/nix-fingerprint-authentication/

  # INFO: [Troubleshoot] when device ever disappear (with `lsusb`)
  # - this can be caused by bios "favoring" Windows - so might as well disable on Windows side
  # - fixing is possible by resetting the bios would help https://github.com/Sabrina-Fox/WM2-Help?tab=readme-ov-file#potential-fix-for-touchscreen-or-other-hardwareeg-fingerprint-sensor-related-issues
  # - but resetting bios also might undo firmware updates (e.g. touchpad, etc.) - in that case retry via https://www.gpd.hk/gpdwinmax2firmwareanddriver

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
