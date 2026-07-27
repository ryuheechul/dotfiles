;;; tests/client-enhance-tests.el --- ERT regression tests -*- lexical-binding: t; -*-

;; The executable half of client/server lifecycle contract in
;; ../modules/my-custom/client-enhance/config.el. Run via ../bin/run-tests.
;;
;; Tests are grouped by the sections in config.el:
;;   - server-visit-workspace-cycle     (section 1: $EDITOR workspace cycle)
;;   - client-frame-inherits-server-directory (section 2: directory inheritance)
;;   - initial-buffer-killed-on-frame-delete  (section 3: dashboard clone cleanup)
;;   - leaked-file-buffer-cleaned-on-frame-start (section 4: leaked buffer cleanup)
;;
;; The first test requires persp-mode (doom's workspace module) and exercises
;; the full open/close lifecycle.  The others use stubs to isolate the logic
;; from the daemon environment.

(require 'ert)

(ert-deftest client-enhance/server-visit-workspace-cycle ()
  "A server client visit ($EDITOR, `emacsclient -n') opens in its own
workspace via `server-window' (client-enhance/server-window-workspace);
`server-done-hook''s client-enhance/server-done-close-workspace closes it
and returns to the workspace - and buffer - the visit came from."
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

(ert-deftest client-enhance/initial-buffer-killed-on-frame-delete ()
  "Deleting a frame kills the dashboard clone created by
`client-frame-inherit-directory', preventing orphan buffers."
  (let ((frame (selected-frame))
        clone-buf)
    (unwind-protect
        (with-selected-frame frame
          (setq clone-buf (clone-indirect-buffer nil nil))
          (set-frame-parameter frame 'client-enhance/client-initial-buffer
                               clone-buf)
          (client-enhance/kill-initial-buffer-on-frame-delete frame)
          (should-not (buffer-live-p clone-buf))
          (should (memq #'client-enhance/kill-initial-buffer-on-frame-delete
                        delete-frame-functions)))
      (when (and clone-buf (buffer-live-p clone-buf))
        (kill-buffer clone-buf)))))

(ert-deftest client-enhance/leaked-file-buffer-cleaned-on-frame-start ()
  "A file buffer in a new client frame that is also displayed in another
frame is removed from the perspective by `clean-leaked-file-buffers'.
The buffer stays alive globally; only the perspective reference is removed.
The stubs simulate an emacsclient frame with a foreign file buffer."
  (let ((leaked-buf (generate-new-buffer "client-enhance-test-leaked"))
        (intended-buf (generate-new-buffer "client-enhance-test-intended"))
        (removed nil))
    (unwind-protect
        (progn
          ;; Give leaked-buf a file name so it looks like a file buffer
          (with-current-buffer leaked-buf
            (setq buffer-file-name "/tmp/leaked.txt"))
          ;; Simulate the client frame context: leaked-buf is in this
          ;; frame's buffer list and also displayed in another frame.
          (cl-letf (((symbol-function 'frame-parameter)
                     (lambda (_frame parameter)
                       (cond ((eq parameter 'client) 'fake-client)
                             (t nil))))
                    ((symbol-function 'get-buffer-window)
                     (lambda (buf &optional frame)
                       (when (and (eq buf leaked-buf)
                                  frame
                                  (not (eq frame (selected-frame))))
                         'fake-window)))
                    ((symbol-function 'buffer-list)
                     (lambda (&optional _frame)
                       (list intended-buf leaked-buf)))
                    ((symbol-function 'window-buffer)
                     (lambda (&optional _window) intended-buf))
                    ((symbol-function 'frame-list)
                     (lambda () (list (selected-frame) 'other-frame)))
                    ((symbol-function 'persp-remove-buffer)
                     (lambda (&optional buf &rest _)
                       (push buf removed)))
                    ;; Execute the timer lambda immediately in tests
                    ((symbol-function 'run-with-timer)
                     (lambda (_secs _idle fn) (funcall fn))))
            (client-enhance/clean-leaked-file-buffers))
          ;; leaked-buf was removed from perspective; intended-buf was not
          (should (memq leaked-buf removed))
          (should-not (memq intended-buf removed)))
      (when (buffer-live-p leaked-buf) (kill-buffer leaked-buf))
      (when (buffer-live-p intended-buf) (kill-buffer intended-buf)))))

;;; client-enhance-tests.el ends here
