{ ... }:

{
  # this replaces typing password with ssh agent
  # debug with `ssh-add -l`
  security.pam.sshAgentAuth = {
    enable = true;
    # `cat /etc/pam.d/sudo` to debug not `/etc/pam.d/login`
    authorizedKeysFiles = [
      # relying on ../user.nix to set this up
      "/etc/ssh/authorized_keys.d/%u"
    ];
  };

  # (with 1password) why this doesn't work locally but remotely (if remote host configured that)
  # - it's explained at https://developer.1password.com/docs/ssh/agent/compatibility/
  # - the gist is 1password guides you to setup `IdentityAgent` at `~/.ssh/config` instead of `SSH_AUTH_SOCK` environment variable
  # - `../../../zsh/zlogin` integrates this to zsh
  # - furthermore `IdentityAgent` will interfere with client's `SSH_AUTH_SOCK` so just sticking with `SSH_AUTH_SOCK` seems a smoother experience overall
}
