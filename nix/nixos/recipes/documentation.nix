{ pkgs, ... }:

# WARN: is 'man-pages' via `../system-pkgs.nix` not sufficient?

# https://nixos.wiki/wiki/Man_pages
# https://discourse.nixos.org/t/some-manpage-related-stuff-you-might-want-to-turn-on/38835
{
  environment.systemPackages = with pkgs; [
    man-pages
    man-pages-posix
  ];

  documentation = {
    dev.enable = true;
    man.generateCaches = true;
    nixos.includeAllModules = true;
  };
}
