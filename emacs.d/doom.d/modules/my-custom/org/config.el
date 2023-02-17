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
