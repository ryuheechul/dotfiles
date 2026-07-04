;;; $DOOMDIR/modules/my-custom/org/config.el -*- lexical-binding: t; -*-

;; configs that customize anything for org mode

;; extending org-protocol
;; see https://github.com/xuchunyang/setup-org-protocol-on-mac/issues/6 in case any issues of opening =org-protocol://= on macOS
;; also there seems to be a limitation on org-protocol picking up things when Emacs.app is not already running
;; that is mitigated by a hack via ../../../../../bin/path/darwin/emacs-opff at least on macOS
(after! org-protocol
        ;; to support https://github.com/ksqsf/logseq-open-in-emacs
        (add-to-list 'org-protocol-protocol-alist
                     '("org-find-file" :protocol "find-file" :function org-protocol-find-file :kill-client nil))

        (defun org-protocol-find-file-fix-wsl-path (path)
          "If inside WSL, change Windows-style paths to WSL-style paths."
          (if (null (getenv "WSL_DISTRO_NAME"))
            path
            (save-match-data
              (if (/= 0 (string-match "^\\([a-zA-Z]\\):\\(/.*\\)" path))
                path
                (let ((volume (match-string-no-properties 1 path))
                      (abspath (match-string-no-properties 2 path)))
                  (format "/mnt/%s%s" (downcase volume) abspath))))))

        (defun org-protocol-find-file (fname)
          "Process org-protocol://find-file?path= style URL."
          (let ((f (plist-get (org-protocol-parse-parameters fname nil '(:path)) :path)))
            (find-file (org-protocol-find-file-fix-wsl-path f))
            (raise-frame)
            (select-frame-set-input-focus (selected-frame)))))

;;; enhancing org-mode experience

;; enable real-auto-save-mode for org-mode
(add-hook 'org-mode-hook 'real-auto-save-mode)

;; why not
(setq org-startup-with-inline-images t)

;; org-modern (via :lang (org +pretty)) restyles tables with box faces and
;; pixel-width display specs (`org-modern-table-vertical' is literally
;; measured in pixels) - TTY frames can't render those, so tables come out
;; mangled in terminal emacs while plain org tables are already aligned as
;; text. keep the styling in GUI only: org-modern builds its font-lock
;; keywords at mode-enable time reading `org-modern-table' (org-modern.el
;; line ~755), so let-binding it around the enable decides per buffer.
;; caveat: the decision sticks to the buffer, judged by the frame it was
;; opened in - a buffer shown by GUI and TTY frames of the SAME process at
;; once keeps whichever look it got first (org-mode-restart re-decides);
;; in practice Emacs.app and the TTY daemon are separate processes, so
;; each side consistently gets its own
(defadvice! my/org-modern-table-gui-only-a (fn &rest args)
  "Enable org-modern's table styling only under a graphical frame."
  :around #'org-modern-mode
  (let ((org-modern-table (and org-modern-table (display-graphic-p))))
    (apply fn args)))
