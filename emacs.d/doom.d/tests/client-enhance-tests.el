;;; tests/client-enhance-tests.el --- ERT regression tests -*- lexical-binding: t; -*-

;; The executable half of client/server lifecycle contract in
;; ../modules/my-custom/client-enhance/config.el. Run via ../bin/run-tests.

(require 'ert)

(ert-deftest client-enhance/server-visit-workspace-cycle ()
  "A server client visit ($EDITOR, `emacsclient -n') opens in its own
workspace via `server-window' (client-enhance/server-window-workspace);
`server-done-hook''s client-enhance/server-done-close-workspace closes it
and returns to the workspace - and buffer - the visit came from,
mirroring nvim's --remote-tab-wait tab-in/tab-out."
  (skip-unless (bound-and-true-p persp-mode))
  (let* ((origin-ws (+workspace-current-name))
         ;; star name = doom-unreal, like a vterm/ghostel terminal
         (term-buf (generate-new-buffer "*client-enhance-test-origin*"))
         (client-buf (generate-new-buffer "client-enhance-test-visit"))
         summon-ws)
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer term-buf)
          ;; through the variable, so the wiring (setq server-window ...)
          ;; is under test too, not just the function
          (funcall server-window client-buf)
          (setq summon-ws (+workspace-current-name))
          (should-not (equal summon-ws origin-ws))
          (should (eq (window-buffer (selected-window)) client-buf))
          (should (equal (buffer-local-value 'client-enhance/--server-workspace
                                             client-buf)
                         summon-ws))
          (with-current-buffer client-buf
            (client-enhance/server-done-close-workspace))
          (should (equal (+workspace-current-name) origin-ws))
          ;; the daemon's initial frame is unmanaged by persp, so the
          ;; origin window conf (term-buf showing) can't be asserted
          ;; headlessly - but the dashboard-swap regression can: the
          ;; kill-CURRENT-workspace path would have swapped the window
          ;; to the fallback buffer and the terminal must survive
          (should-not (eq (window-buffer (selected-window))
                          (doom-fallback-buffer)))
          (should (buffer-live-p term-buf))
          (should-not (+workspace-exists-p summon-ws)))
      (when (and summon-ws (+workspace-exists-p summon-ws))
        (+workspace-kill summon-ws))
      (unless (equal (+workspace-current-name) origin-ws)
        (+workspace-switch origin-ws))
      (when (buffer-live-p client-buf) (kill-buffer client-buf))
      (when (buffer-live-p term-buf) (kill-buffer term-buf)))))

(ert-deftest client-enhance/tty-client-frames-isolated-not-shared-main ()
  "Regression (2026-07-15): a tty `emacsclient -nw' client frame must get its
OWN workspace, never the shared `main' - doom's default lands the first (and
no-file) client in main, which other clients then share, leaking buffers /
cursor / quit across terminals. The override is installed on the emacsclient
frame-init hook; a real tty client frame can't be created headlessly, so this
guards the wiring, not the live frame."
  (skip-unless (bound-and-true-p persp-mode))
  (should (eq persp-emacsclient-init-frame-behaviour-override
              #'client-enhance/isolate-client-frame)))

(ert-deftest client-enhance/client-frame-inherits-server-directory ()
  "A no-file client inherits its caller directory without shell wrappers."
  (let ((client-dir (make-temp-file "client-enhance-client-dir-" t))
        (buf (generate-new-buffer "*client-enhance-client-directory*"))
        client-buf)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buf)
          (with-current-buffer buf
            (setq-local default-directory "/tmp/"))
          ;; server.el stores the directory sent by emacsclient on the
          ;; selected frame's client process.
          (cl-letf (((symbol-function 'frame-parameter)
                     (lambda (_frame parameter)
                       (and (eq parameter 'client) 'client)))
                    ((symbol-function 'doom-fallback-buffer)
                     (lambda () buf))
                    ((symbol-function 'process-get)
                     (lambda (_process property)
                       (and (eq property 'server-client-directory) client-dir))))
            (client-enhance/client-frame-inherit-directory))
          (setq client-buf (window-buffer (selected-window)))
          (should-not (eq client-buf buf))
          (should (equal (buffer-local-value 'default-directory client-buf)
                         (file-name-as-directory client-dir))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (when (and (buffer-live-p client-buf) (not (eq client-buf buf)))
        (kill-buffer client-buf))
      (delete-directory client-dir))))

(ert-deftest client-enhance/client-frame-workspace-is-reclaimed ()
  "Deleting an isolated TTY client frame removes only its #N workspace."
  (skip-unless (bound-and-true-p persp-mode))
  (let ((frame (selected-frame))
        ws)
    (unwind-protect
        (with-selected-frame frame
          (+workspace/new)
          (setq ws (+workspace-current-name))
          (set-frame-parameter frame 'client-enhance/client-workspace ws)
          (client-enhance/reclaim-client-workspace frame)
          (should-not (+workspace-exists-p ws))
          (should (memq #'client-enhance/reclaim-client-workspace
                        delete-frame-functions)))
      (when (and ws (+workspace-exists-p ws))
        (+workspace-kill ws)))))

;;; client-enhance-tests.el ends here
