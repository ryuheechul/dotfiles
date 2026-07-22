;;; $DOOMDIR/modules/my-custom/client-enhance/config.el -*- lexical-binding: t; -*-

(require 'server)

;; A no-file client starts on Doom's shared dashboard. Clone it before making
;; its caller directory buffer-local, so one client's directory cannot leak to
;; the next. server.el stores the emacsclient-supplied directory on its process.
(defun client-enhance/client-frame-inherit-directory ()
  "Give the selected server client buffer its caller's directory."
  (when-let* ((client (frame-parameter nil 'client))
              (dir (process-get client 'server-client-directory)))
    (let* ((window (selected-window))
           (buffer (window-buffer window))
           (client-buffer (if (eq buffer (doom-fallback-buffer))
                              (with-current-buffer buffer
                                (clone-indirect-buffer nil nil))
                            buffer)))
      (unless (eq client-buffer buffer)
        (set-window-buffer window client-buffer)
        (set-frame-parameter nil 'client-enhance/client-initial-buffer
                             client-buffer))
      (with-current-buffer client-buffer
        (setq-local default-directory (file-name-as-directory dir))))))
(add-hook 'server-after-make-frame-hook
          #'client-enhance/client-frame-inherit-directory)

;; $EDITOR client visits (git commit from a terminal here, or the
;; `emacsclient -n' escape hatch) mirror nvim's `--remote-tab-wait': each
;; buffer opens in its own fresh workspace (Doom tab), and finishing it (C-x #
;; / q, see ../../compat/neovim/smart-quit.el) closes that workspace and
;; returns to its origin. Without a server-window function the buffer would
;; replace the calling terminal's own window.
(defvar-local client-enhance/--server-workspace nil
  "Workspace created to show this server client buffer, if any.")

(defun client-enhance/server-window-workspace (buf)
  "Show server client buffer BUF in a fresh workspace of its own,
remembering it buffer-locally so `server-done-hook' can close it."
  (+workspace/new)
  (switch-to-buffer buf)
  (setq client-enhance/--server-workspace (+workspace-current-name)))
(setq server-window #'client-enhance/server-window-workspace)

(defun client-enhance/server-done-close-workspace ()
  "Close the workspace made for this client buffer and return.
Runs off `server-done-hook' while current buffer is the client buffer."
  (when-let* ((ws client-enhance/--server-workspace))
    (setq client-enhance/--server-workspace nil)
    (when (+workspace-exists-p ws)
      ;; Switch away first: killing the current workspace can replace unreal
      ;; buffers (such as the terminal being returned to) with the fallback.
      (when (equal (+workspace-current-name) ws)
        (+workspace-switch
         (or (and (+workspace-exists-p +workspace--last)
                  (not (equal +workspace--last ws))
                  +workspace--last)
             (car (remove ws (+workspace-list-names))))))
      (+workspace-kill ws))))
(add-hook 'server-done-hook #'client-enhance/server-done-close-workspace)

;; Give every TTY ($EDITOR = emacsclient -nw) client frame its own fresh
;; workspace, never shared `main'. Doom's default sends the first/lone client
;; to main, causing clients opened one-at-a-time to share buffers and quit
;; state. GUI clients keep Doom's default behavior.
(defun client-enhance/isolate-client-frame (frame &optional new-frame-p)
  "Give a tty emacsclient FRAME its own workspace; else defer to Doom."
  (if (and persp-mode (not (display-graphic-p frame)))
      (with-selected-frame frame
        (+workspace-switch (format "#%s" (+workspace--generate-id)) t)
        (unless (doom-real-buffer-p (current-buffer))
          (switch-to-buffer (doom-fallback-buffer)))
        (let ((ws (+workspace-current-name)))
          (set-frame-parameter frame 'workspace ws)
          (set-frame-parameter frame 'client-enhance/client-workspace ws))
        (persp-set-frame-buffer-predicate frame)
        (run-at-time 0.1 nil #'+workspace/display))
    (+workspaces-associate-frame-fn frame new-frame-p)))

(defun client-enhance/reclaim-client-workspace (frame)
  "Remove FRAME's isolated client workspace before the frame is deleted."
  (when-let* ((buffer (frame-parameter frame 'client-enhance/client-initial-buffer)))
    (set-frame-parameter frame 'client-enhance/client-initial-buffer nil)
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (when-let* ((ws (frame-parameter frame 'client-enhance/client-workspace)))
    ;; Frame deletion can invoke this hook more than once; make later calls
    ;; no-ops before switching workspaces or deleting anything.
    (set-frame-parameter frame 'client-enhance/client-workspace nil)
    (when (equal (frame-parameter frame 'workspace) ws)
      (set-frame-parameter frame 'workspace nil))
    (when (+workspace-exists-p ws)
      (with-selected-frame frame
        (when (equal (+workspace-current-name) ws)
          (+workspace-switch
           (or (and (+workspace-exists-p +workspace--last)
                    (not (equal +workspace--last ws))
                    +workspace--last)
               (car (remove ws (+workspace-list-names))))))
        (+workspace-kill ws)))))
;; Run before Doom's association cleanup sees a workspace whose frame is gone.
(add-hook 'delete-frame-functions #'client-enhance/reclaim-client-workspace -90)

;; persp-init-frame consults this override for any frame carrying `client'.
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override
        #'client-enhance/isolate-client-frame))
