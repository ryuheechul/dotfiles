# for https://github.com/nresare/pam-ssh-agent
# a Rust re-implementation of `pam_ssh_agent_auth` that:
# - works with macOS's OpenPAM via the `pam-bindings` crate (links -lpam directly)
# - supports more key types (FIDO2/security keys, SSH certificates)
#
# this package's real home is nix/darwin/pkgs/custom (inside the darwin flake
# tree - see ../README.md for why) - nix/pkgs/custom/pam-ssh-agent symlinks back
# here for non-flake consumers like the nixos side
#
# see ../../../modules/security/meta/pam-ssh-agent.nix (darwin) and
# ../../../../nixos/recipes/pam-sshagent.nix (nixos) for usage
#
# build pattern based on the example at
# https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/rust.section.md
# and the repo's own `ssh-agent-switcher` example

{ pkgs }:

with pkgs;

rustPlatform.buildRustPackage rec {
  pname = "pam-ssh-agent";
  version = "0.9.7";

  src = fetchFromGitHub {
    owner = "nresare";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-U4q4LJVCota0jtkFzuWID5shs6ZRJBaxG9c6bSGlWxQ=";
  };

  # `cargoHash` (what the other custom rust packages use) didn't work here:
  # nixpkgs' fetchCargoTarball downloads every crate from the crates.io *api*
  # host (https://crates.io/api/v1/crates/<name>/<version>/download), which 403s
  # non-browser user agents from this machine; `static.crates.io` serves
  # everything, and `cargoLock` fetches via importCargoLock (no api host), so
  # use that instead
  cargoLock = {
    lockFileContents = builtins.readFile "${src}/Cargo.lock";
  };

  # the crate links `-lpam` directly; on darwin nixpkgs aliases `pam` to
  # openpam (same approach as nixpkgs' pam_reattach, the module nix-darwin's
  # reattach option uses), on linux it resolves to linux-pam
  buildInputs = [ pam ];

  # the repo pins a specific rust toolchain via `rust-toolchain.toml` which cargo
  # would try to resolve through rustup (not available in nix builds) - rustPlatform
  # already pins the toolchain so just drop the file
  preConfigure = ''
    rm -f rust-toolchain.toml
  '';

  # cdylib-only crate so `cargo install` has nothing to install
  # on darwin cargo produces a `.dylib` - name it `.so` like other pam modules
  # (mirrors how pam-watchid installs its dylib)
  # artifacts land under target/<triple>/release (no cargoTargetDir var exists)
  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib
    lib="$(find target -name 'libpam_ssh_agent.dylib' -o -name 'libpam_ssh_agent.so' | head -n 1)"
    install -m 0755 "$lib" "$out/lib/pam_ssh_agent.so"
    runHook postInstall
  '';

  meta = {
    homepage = "https://github.com/nresare/pam-ssh-agent";
    description = "PAM module that authenticates using the ssh-agent (Rust re-implementation of pam_ssh_agent_auth)";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
