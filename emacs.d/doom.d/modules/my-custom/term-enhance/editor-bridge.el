;;; $DOOMDIR/modules/my-custom/term-enhance/editor-bridge.el -*- lexical-binding: t; -*-

;; Terminal-to-editor operations shared by Ghostel and VTerm.

;; ghostel-with-cmd/vterm-with-cmd's "quick nvim" terminal needs to be
;; shown in front of everything, not disturb any other window, and be
;; disposable - properties true floating (child frames, e.g. posframe)
;; would give for free, but only in a graphical frame; this needs to work
;; in TTY too (where a second frame surface can't be layered at all), so
;; instead: save the window configuration, take over the whole frame
;; (mirroring term-enhance/mux-zoom's save/delete-other-windows in ./mux.el,
;; not reinventing it), and restore automatically once the terminal
;; exits. Nothing else's window layout is ever touched - just not
;; displayed while the modal is up, then restored exactly as it was
(defvar term-enhance/--quick-editor-wconf nil
  "Window configuration to restore once the quick-editor terminal exits.")

(defun term-enhance/open-in-modal (buffer-name display-fn)
  "Save the window configuration, call DISPLAY-FN (which should display
BUFFER-NAME however it wants), then force that window to fill the whole
frame regardless. `delete-other-windows' up front isn't enough on its
own - it only clears windows that already existed *before* DISPLAY-FN
runs, not a side/bottom window some other display-buffer-alist rule
creates *while* DISPLAY-FN displays the buffer (confirmed: this is
exactly why it came out as a right-side/bottom-50% pane instead of full
frame). Restored once the resulting terminal exits (see
term-enhance/close-quick-editor-wconf-on-exit)."
  (setq term-enhance/--quick-editor-wconf
        (unless (one-window-p) (current-window-configuration)))
  (funcall display-fn)
  (when-let* ((win (get-buffer-window buffer-name)))
    (select-window win)
    (delete-other-windows)))

;; hooked into both ghostel-exit-functions/vterm-exit-functions below
;; (same (buf event) signature term-enhance/mux-close-window-on-exit
;; uses further down) - restores regardless of whichever hook order they
;; end up running in; set-window-configuration overwrites the whole
;; frame's window tree, so it doesn't matter if the other hook already
;; tried (and safely no-op'd on) deleting this buffer's window first
(defun term-enhance/close-quick-editor-wconf-on-exit (buf _event)
  (when (and buf
             (string-match-p "^\\*quick-editor-" (buffer-name buf))
             term-enhance/--quick-editor-wconf)
    (set-window-configuration term-enhance/--quick-editor-wconf)
    (setq term-enhance/--quick-editor-wconf nil)))

;; `find-file-other-window' doesn't guarantee reusing a SPECIFIC
;; existing window, only that it avoids the selected one; with an editor
;; window above and a terminal below, it can still split a third window
;; in between rather than reusing the editor one. This targets "the
;; first non-terminal window" explicitly instead - shared by the
;; find-file bridge and the `e' picker below
(defun term-enhance/editor-window ()
  "First non-terminal window in the frame, or nil."
  (cl-find-if (lambda (w)
                (not (memq (buffer-local-value 'major-mode (window-buffer w))
                           '(vterm-mode ghostel-mode))))
              (window-list)))

;; bridge calls run with the calling terminal's buffer current, and a
;; tramp terminal's shell hands over a bare remote-side path - resolve
;; it against that buffer's remote prefix BEFORE any window switching,
;; or the host would open its own same-looking local file
(defun term-enhance/from-caller-fs (file)
  "FILE as the calling terminal's filesystem sees it."
  (if-let* ((remote (file-remote-p default-directory)))
      (concat remote file)
    file))

;; for ../../../shell/source.zsh's `find-file' shell function
(defun term-enhance/find-file-editor-window (file)
  "Open FILE by reusing the first non-terminal window in the frame
(switching its buffer), falling back to `find-file-other-window' if
every window is currently a terminal."
  (let ((file (term-enhance/from-caller-fs file)))
    (if-let* ((win (term-enhance/editor-window)))
        (progn
          (select-window win)
          (find-file file))
      (find-file-other-window file))))

;; for `e' with no args in a shell inside this emacs: run the
;; interactive find-file picker (vertico, same as SPC f f) with the
;; editor window selected, rooted at DIR. deferred via run-at-time so
;; the minibuffer isn't entered from inside the bridge's process filter
(defun term-enhance/find-file-picker-editor-window (dir)
  ;; resolve against the caller's remote prefix NOW - the deferred
  ;; lambda runs with some other buffer current
  (let ((dir (term-enhance/from-caller-fs dir)))
    (run-at-time 0 nil
                 (lambda ()
                   (when-let* ((win (term-enhance/editor-window)))
                     (select-window win))
                   (let ((default-directory (file-name-as-directory dir)))
                     (call-interactively #'find-file))))))


(defun term-enhance/nvim-open-cmd (file-name line-number)
  ;; `my_nvim_forget_line_number=1' enables opening the same line in neovim as emacs
  (concat "my_nvim_forget_line_number=1 nvim +" line-number " -- " file-name))

(defun term-enhance/current-file-and-line ()
  "Return (FILE-NAME . LINE-NUMBER) for the current buffer, tramp-aware."
  ;; `file-remote-p' is core (no tramp load needed for local files, and for a
  ;; remote buffer tramp is already loaded)
  (cons (if (file-remote-p buffer-file-name)
            (tramp-file-local-name buffer-file-name)
          buffer-file-name)
        (number-to-string (line-number-at-pos))))
