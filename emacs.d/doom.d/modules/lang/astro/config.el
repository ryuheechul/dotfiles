;;; lang/astro/config.el -*- lexical-binding: t; -*-

(if (modulep! :tools treesit-support)
    (after! treesit-langs
      (use-package! astro-ts-mode
        :init
        (when (modulep! +lsp)
          (add-hook 'astro-ts-mode-hook #'lsp! 'append)))))


(set-formatter! 'prettier-astro
                '("npx" "prettier" "--parser=astro"
                  (apheleia-formatters-indent "--use-tabs" "--tab-width" 'astro-ts-mode-indent-offset))
                :modes '(astro-ts-mode))


(use-package! lsp-tailwindcss
  :when (modulep! +lsp)
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
(when (modulep! +lsp)
  (add-hook 'markdown-mode-local-vars-hook #'lsp! 'append))
