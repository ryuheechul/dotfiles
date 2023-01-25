# This file is a complete copy of https://github.com/ivankovnatsky/nixos-config/blob/abeb675d9ae62775f8d1a35cb876a77988d55b73/modules/darwin/pam.nix

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.security.pamCustom;

  # Implementation Notes
  #
  # We don't use `environment.etc` because this would require that the user manually delete
  # `/etc/pam.d/sudo` which seems unwise given that applying the nix-darwin configuration requires
  # sudo. We also can't use `system.patchs` since it only runs once, and so won't patch in the
  # changes again after OS updates (which remove modifications to this file).
  #
  # As such, we resort to line addition/deletion in place using `sed`. We add a comment to the
  # added line that includes the name of the option, to make it easier to identify the line that
  # should be deleted when the option is disabled.
  mkSudoTouchIdAuthScript = isEnabled:
    let
      file = "/etc/pam.d/sudo";
      option = "security.pamCustom.enableSudoTouchIdAuth";
      sed = "${pkgs.gnused}/bin/sed";
    in
    ''
      ${if isEnabled then ''
        # Enable sudo Touch ID authentication, if not already enabled
        if ! grep 'pam_tid.so' ${file} > /dev/null; then
          ${sed} -i '2i\
        auth       optional       /opt/homebrew/lib/pam/pam_reattach.so # nix-darwin: ${option} \
        auth       sufficient     pam_tid.so # nix-darwin: ${option}
          ' ${file}
        fi
      '' else ''
        # Disable sudo Touch ID authentication, if added by nix-darwin
        if grep '${option}' ${file} > /dev/null; then
          ${sed} -i '/${option}/d' ${file}
        fi
      ''}
    '';
in

{
  options = {
    security.pamCustom.enableSudoTouchIdAuth = mkEnableOption ''
      Enable sudo authentication with Touch ID
      When enabled, this option adds the following line to /etc/pam.d/sudo:
          auth       optional       /opt/homebrew/lib/pam/pam_reattach.so
          auth       sufficient     pam_tid.so
      (Note that macOS resets this file when doing a system update. As such, sudo
      authentication with Touch ID won't work after a system update until the nix-darwin
      configuration is reapplied.)
    '';
  };

  config = {
    system.activationScripts.extraActivation.text = ''
      # PAM settings
      echo >&2 "setting up pam..."
      ${mkSudoTouchIdAuthScript cfg.enableSudoTouchIdAuth}
    '';
  };
}
