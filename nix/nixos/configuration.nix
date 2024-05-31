# this file will "replace" /etc/nixos/configuration.nix
# see `../../bootstrap/foundation/nixos/switch-nixos.sh` for more info
# manual is at https://nixos.org/manual/nixos/stable/#sec-modularity
# use `nixos-option` to debug
{ pkgs
, ...
}:

let
  system-pkgs = import ./system-pkgs.nix { pkgs = pkgs; };
  shell-init = ''
    export my_system_nixos="1";
  '';
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
  # https://anarc.at/blog/2022-01-23-chrony/
  # https://chrony-project.org/comparison.html
  services.chrony.enable = true;

  # https://nixos.wiki/wiki/Fonts
  fonts = {
    fontDir.enable = true;
    packages = with pkgs;[
      noto-fonts-cjk # Beautiful and free fonts for CJK languages
      noto-fonts-emoji # Color and Black-and-White emoji fonts
    ];
  };

  environment.systemPackages = system-pkgs;
  programs.zsh = {
    enable = true;
    shellInit = shell-init;
  };
  programs.bash = {
    shellInit = shell-init;
  };

  # prevent going buck wild
  nix.settings.max-jobs = 4;

  # don't include anything here that is not common for all devices
  # customize via ./mix-and-match.nix
}
