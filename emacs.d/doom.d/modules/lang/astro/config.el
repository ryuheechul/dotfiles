;;; lang/astro/config.el -*- lexical-binding: t; -*-

(when (and (modulep! :tools lsp +eglot)(modulep! +lsp))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(astro-ts-mode . ("astro-ls" "--stdio"
                                    :initializationOptions (:typescript (:tsdk "node_modules/typescript/lib")))))))

;; requires `lsp-install-server' for `lsp-mode' but that might not work on NixOS:
;; or make sure to install it manually and it's discoverable via `$PATH'
(use-package! astro-ts-mode
  :after treesit-langs
  :init
  (when (and (modulep! :tools lsp)(modulep! +lsp))
    (add-hook 'astro-ts-mode-hook #'lsp! 'append)))

(set-formatter! 'prettier-astro
  '("npx" "prettier" "--parser=astro"
    (apheleia-formatters-indent "--use-tabs" "--tab-width" 'astro-ts-mode-indent-offset))
  :modes '(astro-ts-mode))

(use-package! lsp-tailwindcss
  :when (and (modulep! :tools lsp -eglot)(modulep! +lsp))
  :after lsp-mode
  :init
  ;; don't forget to install the server via (lsp-install-server)
  (setq! lsp-tailwindcss-add-on-mode t
         ;; find versions at https://marketplace.visualstudio.com/items?itemName=bradlc.vscode-tailwindcss
         lsp-tailwindcss-server-version "0.14.9"
         ;; necessary for tailwind v4 - https://github.com/merrickluo/lsp-tailwindcss/issues/80#issuecomment-2715963134
         lsp-tailwindcss-skip-config-check t)
  :config
  (add-to-list 'lsp-tailwindcss-major-modes 'astro-ts-mode))

;; MDX Support
(add-to-list 'auto-mode-alist '("\\.\\(mdx\\)$" . markdown-mode))
(when (and (modulep! :tools lsp -eglot)(modulep! +lsp))
  (add-hook 'markdown-mode-local-vars-hook #'lsp! 'append))
