;; -*- no-byte-compile: t; -*-
;;; my-custom/org/packages.el

;; install required package

;; when https://www.emacswiki.org/emacs/AutoSave is not enough
(package! real-auto-save)

;; enables =org-gfm-export-to-markdown=
(package! ox-gfm)
;; open a scratch that match the mode to current buffer - https://github.com/ieure/scratch-el
(package! scratch)
