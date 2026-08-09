# ssh-agent based sudo auth without typing the password - the darwin counterpart
# of nixos's `security.pam.sshAgentAuth` (nixos/modules/security/pam.nix).
#
# nixos enables the `pam_ssh_agent_auth` module per service via
# `security.pam.services.<name>.sshAgentAuth`; nix-darwin has no such option, so
# this module composes the line into `security.pam.services.sudo_local.text`
# (types.lines, alongside the touchIdAuth/watchIdAuth/reattach lines).
#
# keys are trusted from `/etc/ssh/pam-ssh-agent/authorized_keys.d/%u` - a
# root-owned dir the activation script keeps in sync from the configured user's
# `~/.ssh/authorized_keys` on every switch - instead of the user-writable
# `~/.ssh/authorized_keys` itself: anyone able to append there would get
# passwordless sudo without any rebuild
# (https://github.com/NixOS/nixpkgs/issues/31611). `%u` expands to the PAM
# *calling* user at auth time, so each user only gets sudo from the keys in
# their own file.
#
# this is a "meta" module: parameterized with the username at import time (see
# ../../../../nixos/recipes/meta/README.md) rather than reading
# `system.primaryUser` - a single-user transition option that will eventually be
# removed, and tying key trust to it would make the config owner's keys valid
# for whoever happens to be primaryUser.
#
# nixos keeps the keys at `/etc/ssh/authorized_keys.d/%u`; that path is
# unusable on darwin because nix-darwin's `system.checks` hard-aborts
# activation whenever the `/etc/ssh/authorized_keys.d` directory exists (a
# security guard, see modules/system/checks.nix) - and sshd on macOS does not
# read that directory anyway.

{ username }: { config, lib, pkgs, ... }:

let
  cfg = config.security.pam.sshAgentAuth;

  # the package lives in the flake tree at ../../../pkgs/custom/pam-ssh-agent
  # (see ../../../pkgs/custom/README.md for why custom packages live inside the
  # flake tree)
  pam-ssh-agent = import ../../../pkgs/custom/pam-ssh-agent { inherit pkgs; };

  # the user the activation script installs keys for - the `username` argument
  # injected at import time (see the header comment)
  userHome = config.users.users.${username}.home or "/Users/${username}";

  # where the activation script installs the keys for `username` - matches the
  # default authorizedKeysFiles once `%u` is expanded for this config's user
  keysFile = "/etc/ssh/pam-ssh-agent/authorized_keys.d/${username}";
in
{
  options.security.pam.sshAgentAuth = {
    enable = lib.mkEnableOption "authenticating using a signature performed by the ssh-agent (pam_ssh_agent)";

    package = lib.mkOption {
      type = lib.types.package;
      default = pam-ssh-agent;
      defaultText = lib.literalExpression "the local pam-ssh-agent package (./pkgs/custom/pam-ssh-agent)";
      description = "The pam_ssh_agent PAM module package to use.";
    };

    authorizedKeysFiles = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "/etc/ssh/pam-ssh-agent/authorized_keys.d/%u" ];
      description = ''
        A list of paths to files in OpenSSH's `authorized_keys` format, containing
        the keys that will be trusted by the `pam_ssh_agent` module.

        The following patterns are expanded when interpreting the path:
        - `%f` and `%H` respectively expand to the fully-qualified and short hostname;
        - `%u` expands to the username;
        - `~` or `%h` expands to the user's home directory.

        Specifying user-writeable files here results in an insecure configuration:
        a malicious process can then edit such an authorized_keys file and bypass
        the ssh-agent-based authentication. The default
        `/etc/ssh/pam-ssh-agent/authorized_keys.d/%u` is root-owned; this module
        keeps it in sync from the configured user's `~/.ssh/authorized_keys` on
        every nix-darwin switch (so key changes require a rebuild, mirroring nixos
        `services.openssh.authorizedKeys.keyFiles` - note the nixos side uses
        `/etc/ssh/authorized_keys.d/%u` there, a path nix-darwin refuses to touch).
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # `pam_ssh_agent` goes first (mkBefore) so a loaded ssh-agent is tried before
    # the touchid/watchid dialogs; `pam_reattach` needs to run before those and is
    # only reached after pam_ssh_agent fails (it is `sufficient`)
    security.pam.services.sudo_local.text = lib.mkBefore ''
      auth       sufficient     ${cfg.package}/lib/pam_ssh_agent.so file=${lib.concatStringsSep ":" cfg.authorizedKeysFiles}
    '';

    system.activationScripts.extraActivation.text = lib.mkAfter ''
      # install a root-owned copy of the user's `~/.ssh/authorized_keys` at
      # /etc/ssh/pam-ssh-agent/authorized_keys.d/<user> so only keys that
      # existed at switch time are trusted (changing keys requires a nix-darwin
      # switch) - mirrors nixos `services.openssh.authorizedKeys.keyFiles` in
      # ../../../../nixos/user.nix
      install -d -m 0755 /etc/ssh/pam-ssh-agent/authorized_keys.d
      if [ -f "${userHome}/.ssh/authorized_keys" ]; then
        install -m 0644 -o root -g wheel "${userHome}/.ssh/authorized_keys" "${keysFile}"
      else
        rm -f "${keysFile}"
      fi
    '';

    assertions = [
      {
        assertion = cfg.authorizedKeysFiles != [ ];
        message = ''
          `security.pam.sshAgentAuth.enable` requires `authorizedKeysFiles` to be a non-empty list.
        '';
      }
    ];

    warnings =
      lib.optional
        (lib.any (s: lib.hasPrefix "%h" s || lib.hasPrefix "~" s) cfg.authorizedKeysFiles)
        ''
          security.pam.sshAgentAuth.authorizedKeysFiles contains files in the user's home directory.

          Specifying user-writeable files there results in an insecure configuration:
          a malicious process can then edit such an authorized_keys file and bypass the
          ssh-agent-based authentication.
          See https://github.com/NixOS/nixpkgs/issues/31611
        '';
  };
}
