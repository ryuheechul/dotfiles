;;; compat/neovim/config.el -*- lexical-binding: t; -*-

;; This layer exist to reduce the gap of neovim's default behavior + my config at =../../../../../nvim/=
;; There is also =../../my-custom/morevil/= that is more concerned on muscle memory (via keybindings);
;; when this module is more concerned in a bigger system layer

;;;; turns out this is not necessary here as the shell will take care of it
;;;; now it's taken care of from =../../../../../zsh/path/set-basic=
;; piggyback installed executable by Neovim!
;; mostly to support smoother experience for lsp - see =../../tools/lsp-support/= for more
;; (let ((mason-path (concat (getenv "XDG_DATA_HOME") "/nvim/mason/bin"))
;;       (lspx-path (concat (getenv "my_dot_d") "/bin/path/lspx")))
;;   (dolist (path (list mason-path lspx-path))
;;     (add-to-list 'exec-path path)))
;; see also =../../../shell/source.zsh= and =../../../../../nix/home/programs/shells.nix==
;;;;

;; fuzzy match completions!!!
(after! orderless ;; orderless is cool and all but I'm used to fuzzy matching more ...
  (defun hotfuzz-setup ()
    "Set up `hotfuzz'."
    (unless (memq 'hotfuzz completion-styles)
      (push 'hotfuzz completion-styles)))

  ;; to overtake orderless set by :completion vertico
  (use-package! hotfuzz
    :config
    (hotfuzz-setup)))

;; to make this to resemble the completion experience from Neovim
(after! corfu
  ;; let's not make the enter key embarassing when there is no candidate
  (setq corfu-preselect 'first))
