{
  config,
  pkgs,
  ...
}:

# debug services at `~/.config/systemd/user/`

let
  emacs = import ./emacs.nix { pkgs = pkgs; };
  opener = import ./opener.nix { pkgs = pkgs; };
  syncthing = import ./syncthing.nix { pkgs = pkgs; };
  tmux-watch-theme-change = import ./tmux-watch-theme-change.nix {
    config = config;
    pkgs = pkgs;
  };
in
{
  # https://nix-community.github.io/home-manager/index.html#_how_to_set_up_a_configuration_for_multiple_users_machines
  imports = [
    emacs
    opener
    syncthing
    tmux-watch-theme-change
  ];

  # if anything doesn't want to belong to its own file it can be added below
}
