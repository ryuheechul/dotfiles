;;; $DOOMDIR/modules/my-custom/lab/config.el -*- lexical-binding: t; -*-

;; configs that where I can freely experiment without concerning about which specific module to manage the code yet

;; if this issue, https://github.com/magit/with-editor/issues/62 appears
;; these might resolve the issue and/or run `doom sync' to see if that helps
;; (when IS-MAC
;;   (if (and (eq window-system nil)
;;            (eq with-editor-emacsclient-executable nil))
;;       (setq with-editor-emacsclient-executable
;;             (shell-command-to-string "command -v emacsclient"))))


;; necessary for local but in tramp case, the shell has to set this up
;; but in local case, eglot will only respect this one instead of the shell one
(add-to-list 'exec-path (concat (getenv "my_dot_d") "/bin/path/emacs"))
