;;; lang/svelte/config.el -*- lexical-binding: t; -*-

(use-package! svelte-ts-mode
  :after treesit-langs
  :init
  (when (and (modulep! :tools lsp)(modulep! +lsp))
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs '(svelte-ts-mode . ("svelteserver" "--stdio"))))
    (add-hook 'svelte-ts-mode-hook #'lsp! 'append)))
