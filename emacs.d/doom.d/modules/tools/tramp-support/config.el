;;; tools/tramp-support/config.el -*- lexical-binding: t; -*-


;; https://www.gnu.org/software/tramp/
;; https://www.emacswiki.org/emacs/TrampMode#h5o-33
;; https://nixos.wiki/wiki/Emacs#Cannot_find_all_binaries_on_a_remote_system_with_TRAMP
(after! tramp-sh
  ;; (setq tramp-histfile-override nil) ;; uncomment this to debug (I manually undo histfile from `../../zsh/zshrc` to exclude history from `dumb` ones but not the interactive ones)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (if (getenv "LSP_USE_PLISTS")
      (add-to-list 'tramp-remote-process-environment "LSP_USE_PLISTS=true")))

;;;; tweaks here below are to support better lsp experience that is powered along side with ../lsp-support/

(when (modulep! :editor format)
  ;; - to prevent apheleia to reject formatting with tramp when eglot can handle it just fine
  ;;   - possible thanks to https://github.com/radian-software/apheleia/pull/76
  ;; - remote cause it to work in sync not async which blocks for a while!!!
  ;;   - I also turns apheleia off for lsp powered editing via ../lsp-support/ so this shouldn't apply for that case
  (setq apheleia-remote-algorithm 'remote))

(when (modulep! :tools direnv)
  ;; so that things like this, https://devenv.sh/common-patterns/#add-a-directory-to-path can be ensured on the remote side too
  (setq envrc-remote t))
;; (unrelated to tramp), if envrc stops loading automatically, (envrc-reload) still works,
;; probably something must have been changed outside of emacs, this might gets fixed via running =(progn (envrc-deny) (envrc-allow))=
