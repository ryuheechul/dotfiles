{ src, name ? src }:
let
  path = (import ./alt-src-to-channel.nix) src;
in
"${name}=${path}"
