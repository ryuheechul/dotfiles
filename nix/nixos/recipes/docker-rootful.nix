{ ... }:

# https://wiki.nixos.org/wiki/Docker
# for a rootlees docker, `./docker.nix`
# see why rootless docker is preferred for better security - https://www.redhat.com/en/blog/understanding-root-inside-and-outside-container
# which can be tested with below;
# `docker run --privileged -it -v /var:/host/var ubuntu touch /host/var/touched-on-host && ls -lah /var/touched-on-host`
# and `docker run --privileged -it -v /var:/host/var ubuntu rm /host/var/touched-on-host` can delete the artifact to clean up
{
  virtualisation.docker.enable = true;
}
