# ssh-agent based sudo auth without typing the password - the darwin counterpart
# of nixos's `security.pam.sshAgentAuth` (nixos/modules/security/pam.nix).
#
# nixos enables the `pam_ssh_agent_auth` module per service via
# `security.pam.services.<name>.sshAgentAuth`; nix-darwin has no such option, so
# this module composes the line into `security.pam.services.sudo_local.text`
# (types.lines, alongside the touchIdAuth/watchIdAuth/reattach lines).
#
# nixos keeps the keys at `/etc/ssh/authorized_keys.d/%u`
# (../../../nixos/recipes/pam-sshagent.nix); here the keys module
# (./pam-ssh-agent-keys.nix) owns its own directory instead
# (`security.pam.sshAgentAuthKeys.directory`, default
# /etc/ssh/pam-ssh-agent/authorized_keys.d). sshd never reads that directory;
# only the pam module does. see ./pam-ssh-agent-keys.nix for why this is a
# separate module rather than nix-darwin's native
# `users.users.<name>.openssh.authorizedKeys.keyFiles`.
#
# this module has no default `authorizedKeysFiles`: ./pam-ssh-agent-keys.nix
# (which imports this module) sets it to `<directory>/%u` when
# `security.pam.sshAgentAuthKeys.enable = true` - enabling without wiring
# fails the assertion below.

{ config, lib, pkgs, ... }:

let
  cfg = config.security.pam.sshAgentAuth;

  # the package lives in the flake tree (see ../../pkgs/custom/README.md for
  # why custom packages live there)
  pam-ssh-agent = import ../../pkgs/custom/pam-ssh-agent { inherit pkgs; };
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
      default = [ ];
      description = ''
        A list of paths to files in OpenSSH's `authorized_keys` format, containing
        the keys that will be trusted by the `pam_ssh_agent` module.

        The following patterns are expanded when interpreting the path:
        - `%f` and `%H` respectively expand to the fully-qualified and short hostname;
        - `%u` expands to the username;
        - `~` or `%h` expands to the user's home directory.

        Specifying user-writeable files here results in an insecure configuration:
        a malicious process can then edit such an authorized_keys file and bypass
        the ssh-agent-based authentication. The keys module
        (./pam-ssh-agent-keys.nix, which imports this module) provides a
        root-owned directory (`security.pam.sshAgentAuthKeys.directory`) and
        keeps it in sync from the configured user's `~/.ssh/authorized_keys` on
        every nix-darwin switch; set
        `security.pam.sshAgentAuthKeys.enable = true` to point this option
        at it automatically.
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

    assertions = [
      {
        assertion = cfg.authorizedKeysFiles != [ ];
        message = ''
          `security.pam.sshAgentAuth.enable` requires `authorizedKeysFiles` to be a non-empty list.
          ./pam-ssh-agent-keys.nix sets it when both `security.pam.sshAgentAuthKeys.enable`
          and `security.pam.sshAgentAuthKeys.user` are set (wired as
          `["$directory/%u"]`); otherwise point it at your own root-owned files.
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
