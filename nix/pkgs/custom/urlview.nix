{ pkgs }:

# obviously this is actually urlscan not urlview but i'm using it in place anyway
# ../../../urlscan to view/edit config
# fake the name until https://github.com/tmux-plugins/tmux-urlview/issues/29 gets resolved
pkgs.writeShellScriptBin "urlview" ''
  ${pkgs.urlscan}/bin/urlscan -s "$@"
''
