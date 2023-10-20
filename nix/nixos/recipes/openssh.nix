{ ... }:

{
  services.openssh.enable = true;
  services.openssh.settings.X11Forwarding = true;
  services.openssh.extraConfig = ''
    # for https://github.com/superbrothers/opener#remote-environment
    # to clean the disconnect socket and prevent connection errors
    StreamLocalBindUnlink yes
  '';
}
