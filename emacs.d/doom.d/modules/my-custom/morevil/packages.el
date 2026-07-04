;; -*- no-byte-compile: t; -*-
;;; my-custom/morevil/packages.el

(package! evil-textobj-line)
;; for + / - number increment/decrement (already present in the straight
;; build as a transitive install, but declare it so doom sync's
;; autoremove can never garbage-collect it from under the binding)
(package! evil-numbers)
;; % on language keywords (if/end, tags, ...), not just brackets - the
;; emacs counterpart of nvim's vim-matchup
(package! evil-matchit)
