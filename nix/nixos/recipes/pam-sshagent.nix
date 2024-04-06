{ ... }:

{
  # this replaces typing password with ssh agent
  # debug with `ssh-add -l`
  security.pam.sshAgentAuth = {
    enable = true;
    authorizedKeysFiles = [
      # relying on ../user.nix to set this up
      "/etc/ssh/authorized_keys.d/%u"
    ];
  };
}
