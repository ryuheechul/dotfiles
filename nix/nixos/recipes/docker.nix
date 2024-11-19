{ ... }:

{
  imports = [ ./podman.nix ];
  virtualisation.podman.dockerCompat = true;
  # run `systemctl --user start podman.socket` in case providing the socket API (e.g. for `lazydocker` or `../../pkgs/custom/termimagenator.nix`);
  # setting $DOCKER_HOST will be assisted by `../../../zsh/my_addons/shell_ext` as long as the socket is active
}

# unfortunately, the below is not feasible for my setup due to the use of `systemd-resolved` - https://github.com/NixOS/nixpkgs/issues/231191
# hence just enable the compat for now until the issue is resolved (maybe docker will chose pasta network as default one day just like podman?)

###### commented out section - uncomment the below (and delete the above) in case of reviving
#
# # https://docs.docker.com/engine/security/rootless/
# # https://wiki.nixos.org/wiki/Docker
# # https://github.com/rootless-containers/rootlesskit
# # for a rootful docker, `./docker-rootful.nix`
# # see why rootless docker is preferred for better security - https://www.redhat.com/en/blog/understanding-root-inside-and-outside-container
# {
#   virtualisation.docker.rootless = {
#     # requires to run once, `systemctl --user start docker.service` or wait until rebooting
#     enable = true;
#     setSocketVariable = true; # this probably requires relogin or reboot
#   };
#
#   # ERROR: conflict with systemd-resolved on host - https://github.com/NixOS/nixpkgs/issues/231191
#   # `environment.etc."resolv.conf".mode = "direct-symlink";` is supposedly the workaround but my system is already with value but same issue exist
# }
# also a networking is more challenging with rootless way as you can tell why like here - [Rootful networking with rootless podman containers - DevConf.CZ 2023](https://www.youtube.com/watch?v=S30Kj135-Kc)
