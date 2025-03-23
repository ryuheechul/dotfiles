;; -*- no-byte-compile: t; -*-
;;; lang/astro/packages.el

;; https://github.com/Sorixelle/astro-ts-mode
(package! astro-ts-mode)

(when (and (modulep! :tools lsp -eglot)(modulep! +lsp))
  ;; https://github.com/merrickluo/lsp-tailwindcss
  (package! lsp-tailwindcss
    :recipe (:host github :repo "merrickluo/lsp-tailwindcss")))
