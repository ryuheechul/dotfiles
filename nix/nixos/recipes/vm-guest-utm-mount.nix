# this is to support the case when it's running as a VM via UTM (powered by QEMU)
{ ... }:

{
  ##### my failed attempt to automate mounting in case of webdav
  #
  # services.davfs2.enable = true;
  # services.autofs = {
  #   enable = true;
  #   autoMaster =
  #     let
  #       mapConf = pkgs.writeText "auto" ''
  #         from-host-spice-webdav -fstype=davfs,rw :http\://localhost\:9843
  #       '';
  #     in
  #     ''
  #       /mnt/auto file:${mapConf}
  #     '';
  # };
  #
  # # to prevent autofs to complain about 'opening /etc/davfs2/secrets failed'
  # environment.etc."davfs2/secrets".source = pkgs.writeText "secrets" "";
  #
  ##### end of failed attempt but no worries since VirtFS is (a lot) faster

  # in case VirtFS is being used instead of webdav - https://docs.getutm.app/guest-support/linux/#virtfs
  fileSystems."/mnt/share/from-utm" =
    {
      device = "share";
      fsType = "9p";
      options = [ "trans=virtio" "version=9p2000.L" "rw" "_netdev" "nofail" ];
    };

  # now you can symlink if you like, e.g. `ln -s /mnt/share/for-user ~/shared-with-host`
  fileSystems."/mnt/share/for-user" =
    {
      device = "/mnt/share/from-utm";
      depends = [ "/mnt/share/from-utm" ];
      fsType = "fuse.bindfs";
      # if permission is not fixed by mapping below, you may try `sudo chown -R $USER /mnt/share/for-user`
      # https://docs.getutm.app/guest-support/linux/#fixing-permission-errors
      options = [ "map=501/1000:@20/@1000" "nofail" ];
    };
}
