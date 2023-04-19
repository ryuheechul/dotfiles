let
  # until this merges to upstream, https://github.com/sourcegraph/src-cli
  rev = "nix";
  url = "https://github.com/ryuheechul/src-cli/archive/${rev}.tar.gz";
  tarball = fetchTarball url;
  at-contrib = tarball + "/contrib";
in
(import at-contrib).default
# NOTE: the binary is `src` not `src-cli`
