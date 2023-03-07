{ config, pkgs, ... }:

let
  shells = import ./shells.nix { config = config; };
in
{
  # https://nix-community.github.io/home-manager/index.html#_how_to_set_up_a_configuration_for_multiple_users_machines
  imports = [
    ./home-manager.nix
    shells
    ./direnv.nix
    ./navi.nix
  ];

  # if anything doesn't want to belong to its own file it can be added below
}
