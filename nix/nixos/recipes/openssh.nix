{ ... }:

{
  services.openssh.enable = true;
  services.openssh.settings.X11Forwarding = true;
  services.openssh.extraConfig = ''
    # for https://github.com/superbrothers/opener#remote-environment
    # to clean the disconnect socket and prevent connection errors
    StreamLocalBindUnlink yes
  '';

  # better security with disabling password auth on login
  # comment out in case temporarily allowing password auth
  services.openssh.settings.PasswordAuthentication = false;
}
