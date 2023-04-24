let
  # TODO: change this to a version tag when new release come out
  rev = "1ef7956e2de5f0aea295631f98d6f0326c2b6367";
  url = "https://github.com/sourcegraph/src-cli/archive/${rev}.tar.gz";
  tarball = fetchTarball url;
  at-contrib = tarball + "/contrib";
in
(import at-contrib).default
# NOTE: the binary is `src` not `src-cli`
