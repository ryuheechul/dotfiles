{
  appName, # usually should be the same as the container name if container has its own ts host with automatically generated name
  tailnetName,
  fqdn, # e.g. "http://${host}:${port}"
}:
{ ... }:

{
  services.tailscale = {
    enable = true;
    # allows caddy to issue a cert via tailscale:
    # - https://tailscale.com/blog/caddy
    # - https://caddyserver.com/docs/automatic-https#activation
    # - https://search.nixos.org/options?show=services.tailscale.permitCertUid
    permitCertUid = "caddy";
    # interfaceName = "userspace-networking"; # critical for container operation if `containers.[name].enableTun = true;` is missing (in case of NixOS container usage)
  };

  services.caddy = {
    enable = true;
    virtualHosts."${appName}.${tailnetName}.ts.net".extraConfig = ''
      reverse_proxy ${fqdn}
    '';
  };
}
