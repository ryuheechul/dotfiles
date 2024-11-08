let
  rev = "v0.1.2";
  url = "https://github.com/ryuheechul/termimagenator/archive/${rev}.tar.gz";
  sha256 = "sha256:0pcxfrrm6aqwb1gwj7cancah7qk4w66v9617s4w3zg9dxxbvpqpq";
in
(import (fetchTarball {
  url = url;
  sha256 = sha256;
})).default
