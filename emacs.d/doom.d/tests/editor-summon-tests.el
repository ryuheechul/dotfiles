;;; tests/editor-summon-tests.el --- ERT regression tests -*- lexical-binding: t; -*-

;; the executable half of the "$EDITOR/summon opens as a tab of the
;; host, done closes back to the terminal" contract - the server-window
;; + server-done-hook machinery in
;; ../modules/my-custom/term-enhance/config.el (the elisp twin of
;; nvim's trap-close-for-term; see "Enter anywhere, nest anything" in
;; ../../../docs/philosophy.md) - run via ../bin/run-tests

(require 'ert)

(ert-deftest editor-summon/server-visit-workspace-cycle ()
  "A server client visit ($EDITOR, `emacsclient -n') opens in its own
workspace via `server-window' (term-enhance/server-window-workspace);
`server-done-hook''s term-enhance/server-done-close-workspace closes it
and returns to the workspace - and buffer - the visit came from,
mirroring nvim's --remote-tab-wait tab-in/tab-out."
  (skip-unless (bound-and-true-p persp-mode))
  (let* ((origin-ws (+workspace-current-name))
         ;; star name = doom-unreal, like a vterm/ghostel terminal
         (term-buf (generate-new-buffer "*editor-summon-test-term*"))
         (client-buf (generate-new-buffer "editor-summon-test-visit"))
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
          (should (equal (buffer-local-value 'term-enhance/--server-workspace
                                             client-buf)
                         summon-ws))
          (with-current-buffer client-buf
            (term-enhance/server-done-close-workspace))
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

(ert-deftest editor-summon/bridge-paths-resolve-against-callers-tramp-prefix ()
  "Regression (2026-07-11, pre-existing): `vi`/`e`/`emacs` from a tramp
terminal's remote shell hand the bridge a bare remote-side path; opened
as-is, the HOST's same-looking local file appears instead.
term-enhance/from-caller-fs must prepend the calling buffer's tramp
prefix - and leave local callers' paths alone."
  (let ((default-directory "/ssh:user@somehost:/home/user/"))
    (should (equal (term-enhance/from-caller-fs "/home/user/x.txt")
                   "/ssh:user@somehost:/home/user/x.txt")))
  (let ((default-directory "/tmp/"))
    (should (equal (term-enhance/from-caller-fs "/tmp/x.txt") "/tmp/x.txt"))))

;;; editor-summon-tests.el ends here
