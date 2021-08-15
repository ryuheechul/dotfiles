{ pkgs }:

let
  tag = import ./tag {pkgs=pkgs;};
in
  [
    tag
  ]
