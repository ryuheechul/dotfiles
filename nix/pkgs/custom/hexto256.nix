let
  rev = "6acca36e1c8c313b59d26aff61f79bef3e8a4c4c";
  url = "https://github.com/ryuheechul/hexto256/archive/${rev}.tar.gz";
  sha256 = "sha256:1by5l50r6zjkx7mj6gd2bc1nij8nyw5y9mp9bh08qcj5wr3xlg0c";
in
(import (fetchTarball {
  url = url;
  sha256 = sha256;
})).default
