# keeps a root-owned copy of the user's `~/.ssh/authorized_keys` at
# `<directory>/<user>` (directory option, default
# /etc/ssh/pam-ssh-agent/authorized_keys.d), refreshed on every nix-darwin
# switch, so only keys that existed at switch time are trusted by the
# pam_ssh_agent module (./pam-ssh-agent.nix).
#
# this module imports the pam module (./pam-ssh-agent.nix) and wires
# `security.pam.sshAgentAuth.authorizedKeysFiles` to `<directory>/%u` itself
# when `enable` and `user` are set.
#
# why not the same `users.users.<name>.openssh.authorizedKeys.keyFiles` that
# ../../../nixos/user.nix uses on nixos (which mirrors `~/.ssh/authorized_keys`
# via `builtins.toPath` + `pathExists` at build time)? the two machines are
# evaluated differently:
#
# - nixos: `nixos-rebuild` evaluates configuration.nix as a plain module on the
#   machine (it replaces /etc/nixos/configuration.nix), so reading absolute
#   paths like `/home/<user>/.ssh/authorized_keys` at eval time is allowed
# - darwin: this repo is a flake (`./flake.nix`, switched via
#   `darwin-rebuild switch --flake path:.`), which evaluates purely - reading
#   absolute paths outside the flake tree is forbidden, so the same toPath
#   trick fails. the native option also bakes its files into the store at
#   build time (`/etc/ssh/nix_authorized_keys.d/<user>`, served to sshd via
#   `AuthorizedKeysCommand`, modules/programs/ssh.nix), so the keys would
#   have to live in the repo - a second source of truth next to
#   `~/.ssh/authorized_keys` that only updates on rebuild
#
# so this module keeps its own `directory` and mirrors the user's
# `~/.ssh/authorized_keys` at switch time instead: sudo trusts exactly what
# local sshd does and key rotation needs no config changes. (the same
# security reason applies as on nixos - see below.)
#
# the copy is root-owned (only changeable via a switch) instead of reading the
# user-writeable `~/.ssh/authorized_keys` directly: anyone able to append there
# would get passwordless sudo without any rebuild
# (https://github.com/NixOS/nixpkgs/issues/31611) - the same reason ../../../nixos/user.nix
# builds the keys into `/etc/ssh/authorized_keys.d` on nixos.
#
# `enable` and `user` are set in ../../meta/configuration.nix; both must be
# set for the module to do anything.

{ config, lib, ... }:

let
  cfg = config.security.pam.sshAgentAuthKeys;

  # the user's own file this module copies from
  userHome = config.users.users.${cfg.user}.home or "/Users/${cfg.user}";
  authorizedKeysSource = "${userHome}/.ssh/authorized_keys";

  # where the activation script installs the root-owned copy
  installPath = "${cfg.directory}/${cfg.user}";

  # what the pam module reads (`%u` expands to the pam user at runtime)
  authorizedKeysFile = "${cfg.directory}/%u";
in
{
  imports = [ ./pam-ssh-agent.nix ];

  options.security.pam.sshAgentAuthKeys = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Whether to sync the user's `~/.ssh/authorized_keys` to the root-owned
        `<directory>/<user>` on every nix-darwin switch and point the pam
        module's `security.pam.sshAgentAuth.authorizedKeysFiles` at
        `<directory>/%u`. Only takes effect while `user` is set.
      '';
    };

    user = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        The user whose `~/.ssh/authorized_keys` is synced to the root-owned
        `<directory>/<user>` on every nix-darwin switch. `null` disables the
        syncing.
      '';
    };

    directory = lib.mkOption {
      type = lib.types.str;
      default = "/etc/ssh/pam-ssh-agent/authorized_keys.d";
      description = ''
        The directory this module keeps the root-owned copy of the user's
        `~/.ssh/authorized_keys` in.
      '';
    };
  };

  config = lib.mkIf (cfg.enable && cfg.user != null) {
    security.pam.sshAgentAuth.authorizedKeysFiles = lib.mkDefault [ authorizedKeysFile ];

    system.activationScripts.extraActivation.text = lib.mkAfter ''
      # install a root-owned copy so only keys that existed at switch time
      # are trusted (changing keys requires a nix-darwin switch)
      #
      # 0755 dir + 0644 root:wheel file: world-readable so the pam module can
      # read the keys no matter which uid the sudo PAM stack runs as (the
      # keys are public anyway); root-owned with no write for others, so only
      # a switch can change the trusted keys
      install -d -m 0755 ${cfg.directory}
      if [ -f "${authorizedKeysSource}" ]; then
        install -m 0644 -o root -g wheel "${authorizedKeysSource}" "${installPath}"
      else
        rm -f "${installPath}"
      fi
    '';
  };
}
