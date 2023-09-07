# https://devenv.sh/getting-started/#__tabbed_3_3
let
  version = "0.6.3";
  url = "https://github.com/cachix/devenv/archive/v${version}.tar.gz";
in
(import (fetchTarball url)).default
