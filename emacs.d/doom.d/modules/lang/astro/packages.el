;; -*- no-byte-compile: t; -*-
;;; lang/astro/packages.el

;; https://github.com/Sorixelle/astro-ts-mode
;; NOTE: its autoloads call `treesit-ready-p' at load time without
;; requiring `treesit' first. Required here for `doom sync' itself
;; (which evaluates this file in-process); see autoload.el for why
;; real Doom startup needs it required separately too.
(require 'treesit)
(package! astro-ts-mode)

(when (and (modulep! :tools lsp -eglot)(modulep! +lsp))
  ;; https://github.com/merrickluo/lsp-tailwindcss
  (package! lsp-tailwindcss
    :recipe (:host github :repo "merrickluo/lsp-tailwindcss")))
