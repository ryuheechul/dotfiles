{ ... }:

{
  # this replaces typing password with ssh agent
  # debug with `ssh-add -l`
  security.pam.sshAgentAuth.enable = true;
}
