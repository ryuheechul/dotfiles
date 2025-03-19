;;; my-custom/soom/config.el -*- lexical-binding: t; -*-

;; until this issue is resolved - https://github.com/doomemacs/doomemacs/issues/8277
(use-package! diff-hl
  :hook (doom-first-file . global-diff-hl-mode)
  :config
  ;; Fix Doom disabling vc in remote buffers
  (after! tramp
    (setopt vc-ignore-dir-regexp locate-dominating-stop-dir-regexp))
  ;;;; below is a verbatim copy from https://github.com/doomemacs/doomemacs/blob/466490c252d06f42a9c165f361de74a6e6abad8d/modules/ui/vc-gutter/config.el#L78-L93
  ;; HACK: diff-hl won't be visible in TTY frames, but there's no simple way to
  ;;   use the fringe in GUI Emacs *and* use the margin in the terminal *AND*
  ;;   support daemon users, so we need more than a static `display-graphic-p'
  ;;   check at startup.
  (if (not (daemonp))
      (unless (display-graphic-p)
        (add-hook 'global-diff-hl-mode-hook #'diff-hl-margin-mode))
    (when (modulep! :os tty)
      (put 'diff-hl-mode 'last t)
      (add-hook! 'doom-switch-window-hook
        (defun +vc-gutter-use-margins-in-tty-h ()
          (when (bound-and-true-p global-diff-hl-mode)
            (let ((graphic? (display-graphic-p)))
              (unless (eq (get 'diff-hl-mode 'last) graphic?)
                (diff-hl-margin-mode (if graphic? -1 +1))
                (put 'diff-hl-mode 'last graphic?)))))))))
