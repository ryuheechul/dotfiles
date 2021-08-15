{ pkgs }:

(import ./git.nix {pkgs=pkgs;})
++
(import ./lang/go.nix {pkgs=pkgs;})
++
(import ./custom/default.nix {pkgs=pkgs;})
