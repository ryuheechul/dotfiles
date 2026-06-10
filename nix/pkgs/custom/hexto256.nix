let
  rev = "18cb0aff49ab956d49933ff9f9bac0ab0b22bf07";
  url = "https://github.com/ryuheechul/hexto256/archive/${rev}.tar.gz";
  sha256 = "sha256:13vazzddx96b2gs3al4zwsl1npmaj3cbaki05hnjcysqbw3c2b9a";
in
(import (fetchTarball {
  url = url;
  sha256 = sha256;
})).default
