# accept username
{ username }:
# and return importable nixos module so `../../bootstrap/foundation/nixos/gen-configuration.sh` can stay minimal
{ pkgs, config, ... }:

let
  user =
    # groups might be reflected only after re-login
    let
      # check `./recipes/podman-rootful-docker.nix` for more info
      is-rootful-podman-used-in-place-of-docker =
        config.virtualisation.podman.dockerSocket.enable && !config.virtualisation.docker.enable;
      groups =
        [ ]
        ++ pkgs.lib.optionals config.virtualisation.docker.enable [ "docker" ]
        ++ pkgs.lib.optionals is-rootful-podman-used-in-place-of-docker [ "podman" ]
        ++ pkgs.lib.optionals config.services.syncthing.enable [ "syncthing" ]
        ++ pkgs.lib.optionals config.virtualisation.libvirtd.enable [
          "libvirtd"
          "qemu-libvirtd"
        ]
        ++ pkgs.lib.optionals config.virtualisation.incus.enable [ "incus-admin" ]
        ++ pkgs.lib.optionals config.services.sunshine.enable [ "input" ]
        ++ pkgs.lib.optionals config.services.davfs2.enable [ "davfs2" ];
    in
    with pkgs.lib;
    {
      isNormalUser = mkForce true;
      description = mkForce "user is ${username}";
      # NOTE regarding groups:
      # - changes on groups can be seen right away with `cat /etc/group` but not with `groups`
      # - this will take affect at login time which may require rebooting
      # - https://stackoverflow.com/a/7537275/1570165
      extraGroups = mkForce (
        [
          "networkmanager"
          "wheel"
        ]
        ++ groups
      );
      packages = mkMerge [ ]; # since I already manage my packages via ../pkgs
      # if want a package that is NixOS specific look at ./system-pkgs.nix
      shell = mkForce pkgs.zsh;

      # since `~/.ssh/authorized_keys` is being used here, it seems redundant
      # however this is to make ./recipes/pam-sshagent.nix to be a bit more secure
      # by only allowing the built one on `/etc/ssh/authorized_keys.d/$USER`
      openssh.authorizedKeys.keyFiles =
        let
          authorized_keys = builtins.toPath "/home/${username}/.ssh/authorized_keys";
          authorizedKeyFiles =
            [ ]
            ++ pkgs.lib.optionals (builtins.pathExists authorized_keys) [ authorized_keys ];
        in
        authorizedKeyFiles;
    };
in
{
  # alternative to setting these directly here would be
  # to set this to each module at ./recipes/[module].nix and ./mix-and-match.nix
  # similar to how this file can accept username and return importable module
  users.users."${username}" = user;
  nix.settings.trusted-users = [
    username
  ]; # for devenv to use cachix cache
  programs._1password-gui.polkitPolicyOwners =
    pkgs.lib.optionals config.programs._1password-gui.enable
      [ username ];
}
