{ pkgs, ... }:

let
  emacs = import ./emacs.nix { pkgs = pkgs; };
in
{
  # https://nix-community.github.io/home-manager/index.html#_how_to_set_up_a_configuration_for_multiple_users_machines
  imports = [
    emacs
  ];

  # if anything doesn't want to belong to its own file it can be added below
}
