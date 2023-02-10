let
  rev = "6acca36e1c8c313b59d26aff61f79bef3e8a4c4c";
  url = "https://github.com/ryuheechul/hexto256/archive/${rev}.tar.gz";
in
(import (fetchTarball url)).default
