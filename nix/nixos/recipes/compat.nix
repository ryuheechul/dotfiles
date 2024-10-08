{ ... }:

{
  # what is `nix-ld` and how it works? https://blog.thalheim.io/2022/12/31/nix-ld-a-clean-solution-for-issues-with-pre-compiled-executables-on-nixos/
  programs.nix-ld.enable = true; # rebooting or logout is required after this change is applied due to `NIX_LD` env var only populates via start up
  # debug with `readlink /lib64/ld-linux-x86-64.so.2` and `printenv NIX_LD`
  # known use cases:
  # - to support pre compiled binaries via mason ../../../nvim/lua/plugins/lsp.lua
  # - as a replacement of `steam-run` approach described at https://gist.github.com/ryuheechul/c0dc3c0f39790f9a90404dae75fe2b3f;
  #   via https://github.com/nix-community/nix-ld?tab=readme-ov-file#my-pythonnodejsrubyinterpreter-libraries-do-not-find-the-libraries-configured-by-nix-ld
}
