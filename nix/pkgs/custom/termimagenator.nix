let
  rev = "v0.1.2";
  url = "https://github.com/ryuheechul/termimagenator/archive/${rev}.tar.gz";
in
(import (fetchTarball url)).default
