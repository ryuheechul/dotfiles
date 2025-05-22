{
  # not a fan of static values but it should be OK for this case
  hostAddr ? "192.168.200.1",
  localAddr ? "192.168.200.2",
  containerName ? "ollama",
  # debug with `machinectl list`
  tailnetName, # requires manual login inside the container at least once
}:
# ```nix
# # example use case that can be place at `../../mix-and-match.nix`
# (let
#   tailnetName = "my-ts-net-name"; # excluding "ts.net" at the end
# in
#   import ./containers/meta/ollama-amd.nix { tailnetName = tailnetName; })
# ```
{ ... }:

{
  # so that `containers.${containerName}.enableTun = true;` can be supported
  networking.nat = {
    enable = true;
    internalInterfaces = [ "ve-+" ]; # NAT for all container interfaces
    externalInterface = "wlp1s0"; # Your host's physical interface
  };

  containers.${containerName} = {
    autoStart = true;
    privateNetwork = true;
    hostAddress = hostAddr; # `ip route` to debug
    localAddress = localAddr; # `ip address` to debug
    # so that `services.tailscale.interfaceName = "userspace-networking";` can be avoided
    enableTun = true;
    bindMounts = {
      # ROCk module is loaded
      amdgpu-module = rec {
        # NOTE requires these below on the host:
        # - `boot.kernelModules = [ "amdgpu"]`
        # - or `boot.initrd.kernelModules = [ "amdgpu"]`
        hostPath = "/sys/module/amdgpu";
        mountPoint = hostPath;
      };
      # to access GPU device
      dri = rec {
        hostPath = "/dev/dri";
        mountPoint = hostPath;
      };
      # for clinfo to work
      kfd = rec {
        hostPath = "/dev/kfd";
        mountPoint = hostPath;
      };
    };
    allowedDevices = [
      {
        # nvtop will not be able to find a GPU without it, and probably cause an issue with GPU usage?
        modifier = "rw";
        node = "/dev/dri/renderD128";
      }
      {
        # to prevent the error message below
        # Unable to open /dev/kfd read-write: Operation not permitted
        modifier = "rw";
        node = "/dev/kfd";
      }
    ];

    config =
      { config, ... }:
      {
        imports = [
          (import ../../recipes/meta/ollama-amd.nix { })
          (
            let
              # host and port coming from the result of importing `ollama-amd.nix` above
              host = config.services.open-webui.host;
              port = builtins.toString config.services.open-webui.port;
            in
            import ../../recipes/meta/caddy-over-tailscale.nix {
              appName = containerName;
              tailnetName = tailnetName;
              fqdn = "http://${host}:${port}";
            }
          )
        ];
      };
  };
}
