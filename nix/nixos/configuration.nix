# this file will "replace" /etc/nixos/configuration.nix
# see `../../bootstrap/foundation/nixos/switch-nixos.sh` for more info
{ pkgs
, ...
}:

let
  system-pkgs = import ./system-pkgs.nix { pkgs = pkgs; };
in
{
  # what the heck really is `imports` variable?
  # best answer that I found so far - https://nixos.wiki/wiki/NixOS_modules
  imports =
    [
      # Include the default configuration that is generated on installation as a good default
      /etc/nixos/configuration.nix # will import `/etc/nixos/hardware-configuration.nix` too
    ];

  # https://nixos.wiki/wiki/NTP
  services.ntp.enable = true;

  # https://nixos.wiki/wiki/Fonts
  fonts = {
    fontDir.enable = true;
    fonts = with pkgs;[
      noto-fonts-emoji # Color and Black-and-White emoji fonts
    ];
  };

  environment.systemPackages = system-pkgs;
  programs.zsh.enable = true; # for shell = pkgs.zsh; at ./user.nix

  # prevent going buck wild
  nix.settings.max-jobs = 4;

  # don't include anything here that is not common for all devices
  # customize via ./mix-and-match.nix
}
