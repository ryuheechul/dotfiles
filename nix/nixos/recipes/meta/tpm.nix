username:

{ pkgs, ... }:

# https://nixos.wiki/wiki/TPM
{
  environment.systemPackages = [ pkgs.tpm2-tools ];
  security.tpm2.enable = true;
  security.tpm2.pkcs11.enable = true; # expose /run/current-system/sw/lib/libtpm2_pkcs11.so
  security.tpm2.tctiEnvironment.enable = true; # TPM2TOOLS_TCTI and TPM2_PKCS11_TCTI env variables
  users.users.${username}.extraGroups = [ "tss" ]; # tss group has access to TPM devices
}
