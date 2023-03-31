let
  rev = "0c80e917b50eb934c9324e44961538e0826a6f58";
  # until this merges to upstream, https://github.com/sourcegraph/src-cli
  url = "https://github.com/ryuheechul/src-cli/archive/${rev}.tar.gz";
in
(import (fetchTarball url)).default
