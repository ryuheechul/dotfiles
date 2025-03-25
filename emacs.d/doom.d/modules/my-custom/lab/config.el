;;; $DOOMDIR/modules/my-custom/lab/config.el -*- lexical-binding: t; -*-

;; configs that where I can freely experiment without concerning about which specific module to manage the code yet

;; if this issue, https://github.com/magit/with-editor/issues/62 appears
;; these might resolve the issue and/or run `doom sync' to see if that helps
;; (when IS-MAC
;;   (if (and (eq window-system nil)
;;            (eq with-editor-emacsclient-executable nil))
;;       (setq with-editor-emacsclient-executable
;;             (shell-command-to-string "command -v emacsclient"))))

(when (display-graphic-p)
  (use-package! eaf
    :if (eq system-type 'gnu/linux)
    :custom
    (emacsdirecto)
    (eaf-find-alternate-file-in-dired t)
    (eaf-browser-continue-where-left-off t)
    (eaf-browser-enable-adblocker t)
    (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
    :config

    ;; if you are on an immutable system like NixOS
    (let* ((venv-path (concat doom-emacs-dir ".local/etc/python-venv" ))
           (venv-bin-path (concat venv-path "/bin" )))
      (call-process "bash" nil nil nil "-c" (concat "test -d " venv-path " || python3 -m venv " venv-path " --copies && fix-python --venv " venv-path))
      (add-to-list 'exec-path venv-bin-path)
      (let ((path (getenv "PATH")))
        (unless (cl-search venv-bin-path path)
          (setenv "PATH" (concat venv-bin-path ":" path))))
      ;; relying on =../../../../../zsh/path/set-basic= to ensure the path to take precedence
      (setenv "ADDITIONAL_PATH_FOR_EMACS" venv-bin-path))

    (defun eaf-open-google ()
      "Open Google using EAF."
      (interactive)
      (eaf-open-browser "https://www.google.com"))

    (defalias 'browse-web #'eaf-open-browser)

    ;; (require 'eaf-file-manager)
    ;; ;; (require 'eaf-music-player)
    ;; ;; (require 'eaf-image-viewer)
    ;; ;; (require 'eaf-camera)
    ;; (require 'eaf-demo)
    ;; ;; (require 'eaf-airshare)
    ;; (require 'eaf-terminal)
    ;; (require 'eaf-markdown-previewer)
    ;; ;; (require 'eaf-video-player)
    ;; (require 'eaf-vue-demo)
    ;; ;; (require 'eaf-file-sender)
    ;; (require 'eaf-pdf-viewer)
    ;; ;; (require 'eaf-mindmap)
    ;; ;; (require 'eaf-netease-cloud-music)
    ;; (require 'eaf-jupyter)
    ;; (require 'eaf-org-previewer)
    ;; (require 'eaf-system-monitor)
    ;; ;; (require 'eaf-rss-reader)
    ;; (require 'eaf-file-browser)
    (require 'eaf-browser)
    ;; (require 'eaf-org)
    ;; ;; (require 'eaf-mail)
    ;; (require 'eaf-git)
    ;; (when (display-graphic-p)
    ;;   (require 'eaf-all-the-icons))

    ;; (require 'eaf-evil)
    (define-key key-translation-map (kbd "SPC")
                (lambda (prompt)
                  (if (derived-mode-p 'eaf-mode)
                      (pcase eaf--buffer-app-name
                        ("browser" (if  (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True")
                                       (kbd "SPC")
                                     (kbd eaf-evil-leader-key)))
                        ("pdf-viewer" (kbd eaf-evil-leader-key))
                        ("image-viewer" (kbd eaf-evil-leader-key))
                        (_  (kbd "SPC")))
                    (kbd "SPC"))))))
