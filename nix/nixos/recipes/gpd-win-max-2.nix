{ ... }:

{
  imports = [
    <nixos-hardware/gpd/win-max-2>
  ];

  # regarding fprintd in general:
  # - https://sbulav.github.io/nix/nix-fingerprint-authentication/

  services.fprintd.enable = true;
  # how to enroll: `sudo fprintd-enroll [username]`
  # how to verify: `fprintd-verify`
  # check from gnome setting: "Settings > Users"

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
