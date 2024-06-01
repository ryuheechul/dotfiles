{ pkgs }:

# obviously this is actually urlscan not urlview but i'm using it in place anyway
# ../../../urlscan to view/edit config
# fake the name until https://github.com/tmux-plugins/tmux-urlview/issues/29 gets resolved
let
  wrapped = pkgs.writeShellScriptBin "urlview" ''
    exec ${pkgs.urlscan}/bin/urlscan -s "$@"
  '';
in
pkgs.symlinkJoin {
  name = "urlview";
  paths = [
    wrapped
    pkgs.urlscan
  ];
}

# favoring above instead of below according to https://nixos.wiki/wiki/Nix_Cookbook#Wrapping_packages

# pkgs.writeShellScriptBin "urlview" ''
#   exec ${pkgs.urlscan}/bin/urlscan -s "$@"
# ''
