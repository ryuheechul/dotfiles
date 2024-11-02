{
  network,
  useResolved ? true,
}:
{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports =
    [ ]
    ++ lib.optionals useResolved [
      ../resolved.nix
    ];
  # https://discourse.nixos.org/t/mkmerge-as-the-body-of-a-configuration/9666/2
  config = lib.mkMerge [
    {
      services.zerotierone = {
        enable = true;
        joinNetworks = [
          network
        ];
      };
    }
    # fulfill the wish of ./bin/zerotier-dns-client.sh with automation in case of using systemd-resolved
    (lib.mkIf useResolved {
      systemd.services.zerotier-dns-resolved = {
        wantedBy = [ "multi-user.target" ];
        after = [ "zerotierone.service" ];

        serviceConfig = {
          Type = "oneshot";
        };

        path = [
          config.services.zerotierone.package
          pkgs.jq
        ];
        script = builtins.readFile ./bin/zerotier-dns-client.sh;
      };
    })
    # debug with `systemctl cat zerotier-dns-resolved.service`
  ];
}
