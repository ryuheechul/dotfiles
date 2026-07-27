;;; $DOOMDIR/modules/my-custom/client-enhance/config.el -*- lexical-binding: t; -*-

;; client-enhance: client frame lifecycle for daemon and non-daemon servers
;;
;; `emacsclient' connects to an Emacs server in one of two ways:
;;   - A long-lived daemon (`emacs --daemon'): the server process is the main
;;     Emacs instance; emacsclient creates frames on it (can also start one
;;     via `-a ""' if not running).
;;   - A parent Emacs with `server-start': terminals within Emacs inherit
;;     $EDITOR=emacsclient (set by term-enhance/environment.el), so shell
;;     commands like `git commit' spawn emacsclient back to the parent.
;;
;; In both cases, the server's buffer list, default-directory, and workspace
;; assignments are global state - one client can accidentally "see" another's
;; buffers or directories.
;;
;; This module does NOT override Doom's workspace isolation (the
;; `+workspaces-associate-frame-fn' already gives each new frame its own
;; perspective). Instead, it handles the edges that Doom's default doesn't
;; cover:
;;
;;   1. Workspace cycle: emacsclient opens in its own workspace and closes
;;      it when done, whether from a terminal within Emacs or externally.
;;      -> works for both daemon and non-daemon server
;;
;;   2. Directory inheritance: a no-file client inherits the caller's working
;;      directory so Magit (etc.) opens the right project.
;;      -> works for both daemon and non-daemon server
;;
;;   3. Dashboard clone lifecycle: the clone created for directory inheritance
;;      is cleaned up when the frame is deleted.
;;      -> works for both daemon and non-daemon server
;;
;;   4. Leaked file buffer cleanup: the very first client frame after the main
;;      frame can inherit a file buffer from the main frame's buffer list;
;;      this is removed on frame creation.
;;      -> specific to daemon (first client frame shares "main" workspace)
;;
;; All hooks run from `server-after-make-frame-hook' (frame creation) and
;; `delete-frame-functions' / `server-done-hook' (cleanup).

(require 'server)


;;; ---------------------------------------------------------------------------
;;; 1. Workspace cycle for emacsclient
;;;
;;; When emacsclient connects (either from a terminal within Emacs via
;;; $EDITOR, or from an external terminal to a daemon), it spawns a buffer
;;; that needs to be shown. This hook intercepts the buffer via
;;; `server-window' and opens it in a fresh Doom workspace (tab), so the
;;; caller's view isn't replaced.
;;;
;;; Finishing the edit (C-x # / q) fires `server-done-hook', which closes
;;; the workspace and returns to the caller - same round-trip as nvr's
;;; --remote-tab-wait, built on Emacs's native server/client protocol.
;;;
;;; Flow:
;;;   emacsclient connects (with or without file)
;;;     -> server calls server-window with the buffer
;;;     -> server-window-workspace creates a new workspace, switches to it
;;;     -> user edits, then q (smart-quit) or C-x # (server-edit)
;;;     -> server-done-hook fires server-done-close-workspace
;;;     -> workspace is killed, caller returns to its origin
;;; ---------------------------------------------------------------------------

(defvar-local client-enhance/--server-workspace nil
  "Workspace created to show this server client buffer, if any.
Buffer-local so `server-done-hook' can close exactly the workspace
that was created for this buffer, even across multiple clients.")

(defun client-enhance/server-window-workspace (buf)
  "Show server client buffer BUF in a fresh workspace of its own.
Remembers the workspace name buffer-locally so
`server-done-hook' can close it later."
  ;; Create a new workspace (Doom tab) and switch to it.
  (+workspace/new)
  (switch-to-buffer buf)
  ;; Record which workspace was created, so the cleanup hook knows what
  ;; to kill. Buffer-local: each client buffer tracks its own workspace.
  (setq client-enhance/--server-workspace (+workspace-current-name)))

;; Wire server-window so emacsclient calls go through our workspace function.
;; This is the entry point: without it, emacsclient opens in the current
;; window instead of a dedicated workspace.
(setq server-window #'client-enhance/server-window-workspace)

(defun client-enhance/server-done-close-workspace ()
  "Close the workspace made for this client buffer and return.
Runs off `server-done-hook' while current buffer is the client buffer.
The workspace switch MUST happen before the kill: doom's +workspace/kill
on the current workspace follows its switch with a
`doom-buffer-frame-predicate' check that swaps an \"unreal\" buffer
(e.g. a vterm/ghostel terminal) for the fallback -- showing the
dashboard instead of the terminal we're trying to return to."
  (when-let* ((ws client-enhance/--server-workspace))
    ;; Clear the marker first to make this function idempotent.
    (setq client-enhance/--server-workspace nil)
    (when (+workspace-exists-p ws)
      ;; Switch away from the workspace BEFORE killing it.
      ;; Prefer the last-used workspace (the terminal we came from);
      ;; fall back to any other workspace.
      (when (equal (+workspace-current-name) ws)
        (+workspace-switch
         (or (and (+workspace-exists-p +workspace--last)
                  (not (equal +workspace--last ws))
                  +workspace--last)
             (car (remove ws (+workspace-list-names))))))
      (+workspace-kill ws))))

(add-hook 'server-done-hook #'client-enhance/server-done-close-workspace)


;;; ---------------------------------------------------------------------------
;;; 2. Directory inheritance (daemon + non-daemon server)
;;;
;;; When a no-file emacsclient connects (e.g. bare `emacsclient -nw -a ""'),
;;; the new frame initially shows Doom's shared dashboard
;;; buffer (*doom*). The dashboard's `default-directory' is stale (left over
;;; from whatever the server last did), so Magit would open the wrong project.
;;;
;;; Emacs already receives the caller's directory on the server process
;;; (`server-client-directory'), but only stores it there. This function
;;; persists it on a per-client clone of the dashboard, so each client sees
;;; its own directory without affecting siblings.
;;;
;;; Why a clone? The dashboard is shared across all frames. Making
;;; `default-directory' buffer-local on *doom* itself would leak one client's
;;; directory to the next. An indirect buffer gives each client its own
;;; buffer-local state while sharing the text.
;;;
;;; The clone's name (e.g. " *doom*<2>") differs from *doom*, which means
;;; `+dashboard-reposition-point-h' -- a post-command-hook gated on the
;;; buffer name -- never fires for it. We explicitly move point to the first
;;; button after cloning so the cursor lands on the first menu item.
;;; ---------------------------------------------------------------------------

(defun client-enhance/client-frame-inherit-directory ()
  "Give the selected server client buffer its caller's directory.
For no-file clients, clones the dashboard buffer and makes its
`default-directory' buffer-local to the caller's path. For
file-visiting clients, just sets the directory on the existing buffer."
  (when-let* ((client (frame-parameter nil 'client))
              (dir (process-get client 'server-client-directory)))
    (let* ((window (selected-window))
           (buffer (window-buffer window))
           (client-buffer (if (eq buffer (doom-fallback-buffer))
                              ;; No-file client: clone the dashboard so each
                              ;; client gets its own default-directory.
                              (with-current-buffer buffer
                                (let ((clone (clone-indirect-buffer nil nil)))
                                  ;; Move point to the first menu button.
                                  ;; Without this, the cursor sits at point-min
                                  ;; because +dashboard-reposition-point-h
                                  ;; doesn't recognise the clone by name.
                                  (with-current-buffer clone
                                    (goto-char (point-min))
                                    (forward-button 1 nil nil t))
                                  clone))
                            ;; File-visiting client: buffer is already correct,
                            ;; no clone needed.
                            buffer)))
      ;; If we created a clone, swap it into the window and track it for
      ;; cleanup when the frame is deleted (see kill-initial-buffer below).
      (unless (eq client-buffer buffer)
        (set-window-buffer window client-buffer)
        (set-frame-parameter nil 'client-enhance/client-initial-buffer
                             client-buffer))
      ;; Stamp the caller's directory onto the client buffer.
      (with-current-buffer client-buffer
        (setq-local default-directory (file-name-as-directory dir))))))

(add-hook 'server-after-make-frame-hook
          #'client-enhance/client-frame-inherit-directory)


;;; ---------------------------------------------------------------------------
;;; 3. Dashboard clone lifecycle (daemon + non-daemon server)
;;;
;;; The clone created by client-frame-inherit-directory (section 2) must be
;;; killed when its frame is deleted. Otherwise it lingers as an orphan in
;;; the global buffer list, wasting memory and appearing in buffer pickers.
;;;
;;; This is a simplified version of the old reclaim-client-workspace hook,
;;; which also managed workspace cleanup. Doom's own
;;; +workspaces-delete-associated-workspace-h now handles workspace lifecycle;
;;; we only need to clean up the buffer.
;;; ---------------------------------------------------------------------------

(defun client-enhance/kill-initial-buffer-on-frame-delete (frame)
  "Kill FRAME's dashboard clone if it was created by `client-frame-inherit-directory'.
Called from `delete-frame-functions'. Idempotent: safe if the hook
fires more than once for the same frame."
  (when-let* ((buffer (frame-parameter frame 'client-enhance/client-initial-buffer)))
    ;; Clear the frame parameter first to make this a no-op on repeat calls.
    (set-frame-parameter frame 'client-enhance/client-initial-buffer nil)
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(add-hook 'delete-frame-functions #'client-enhance/kill-initial-buffer-on-frame-delete)


;;; ---------------------------------------------------------------------------
;;; 4. Leaked file buffer cleanup (daemon only)
;;;
;;; Doom's +workspaces-associate-frame-fn gives each new client frame its own
;;; workspace. However, the very first client frame created after the main
;;; frame is open can inherit a file buffer from the main frame's buffer
;;; list. This manifests as: q'ing the last buffer in the client falls back
;;; to the main frame's file instead of quitting, or the buffer appearing in
;;; SPC b b despite being foreign.
;;;
;;; Root cause: the first client frame lands in the "main" workspace (shared
;;; with the server) because +workspaces-associate-frame-fn only creates a
;;; fresh #N workspace when other non-daemon frames already exist. A file
;;; buffer opened in the main frame appears in this shared workspace's
;;; buffer list. We remove it with persp-remove-buffer targeting only the
;;; current frame's perspective, so the owning frame's workspace is unaffected.
;;;
;;; Hook: server-after-make-frame-hook + deferred timer.
;;; Two constraints make a clean synchronous hook impossible:
;;;   1. server-after-make-frame-hook fires inside server--process-filter-1
;;;      where the selected frame may not be the new client frame yet, so
;;;      (get-current-persp) returns nil.
;;;   2. persp-activated-functions is not an option: for the first client
;;;      frame, +workspaces-associate-frame-fn switches to "main" (already
;;;      the current workspace), so +workspace-switch is a no-op and
;;;      persp-activate is never called. The hook simply does not fire.
;;; Covers both TTY and GUI emacsclient frames.
;;; ---------------------------------------------------------------------------

(defun client-enhance/clean-leaked-file-buffers ()
  "Remove file buffers from a new client frame that leaked from another frame.
A file buffer is considered \"leaked\" if it is in this frame's buffer
list AND also displayed in another frame (meaning it belongs to that
frame's workspace). Removed from the current frame's perspective only;
the buffer stays alive in the owning frame.
Runs via a deferred timer on `server-after-make-frame-hook' because the
perspective is not yet accessible synchronously from that hook."
  (when (frame-parameter nil 'client)
    (let ((remove-leaked-buffers
           (lambda ()
             ;; The intended buffer is the one the user explicitly opened
             ;; (or the dashboard clone from section 2). Never touch it.
             (let ((intended (window-buffer (selected-window))))
               (dolist (buf (buffer-list))
                 (when (and (not (eq buf intended))   ; not the intended buffer
                            (buffer-file-name buf)     ; only file buffers
                            (buffer-live-p buf)        ; still alive
                            ;; Displayed in another frame? -> foreign.
                            (cl-some (lambda (f)
                                       (and (not (eq f (selected-frame)))
                                            (get-buffer-window buf f)))
                                     (frame-list)))
                   ;; Remove from this frame's perspective only.
                   (persp-remove-buffer buf)))))))
      ;; Deferred timer is a hack: server-after-make-frame-hook fires
      ;; inside the process filter where (get-current-persp) returns nil,
      ;; and persp-activated-functions never fires for the first client.
      ;; This is the best we found so far; there may be a more
      ;; deterministic hook or persp-mode entry point.
      (run-with-timer 0.1 nil remove-leaked-buffers))))

(add-hook 'server-after-make-frame-hook
          #'client-enhance/clean-leaked-file-buffers)
