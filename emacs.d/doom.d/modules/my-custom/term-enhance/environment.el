;;; $DOOMDIR/modules/my-custom/term-enhance/environment.el -*- lexical-binding: t; -*-

;; Environment inherited by terminal children.

;; Ensure this Emacs exposes a server interface so local child processes can
;; open files in it through emacsclient. Shells inside a directly launched
;; regular Emacs use plain emacsclient as $EDITOR, so give that Emacs a server
;; of its own. A daemon or Emacs with a server process already owns its server
;; identity, so preserve it.
(require 'server)
(let* ((daemon-p (daemonp))
       (server-process-present-p (bound-and-true-p server-process))
       (start-server-p (not (or daemon-p server-process-present-p))))
  (when start-server-p
    ;; Separate regular Emacs processes must not contend for the default name.
    (let ((socket-name (format "server-%d" (emacs-pid))))
      (setq server-name socket-name)
      ;; A server is a lightweight socket interface in this Emacs, not another
      ;; daemon; it lets local emacsclient processes reuse this instance.
      (server-start))))

;; Point local terminal children at this Emacs's server, so their emacsclient
;; opens files here instead of connecting to another default server. A daemon
;; keeps its default socket; a directly launched regular Emacs uses the
;; PID-specific socket ensured above.
(let ((socket-path (expand-file-name server-name server-socket-dir)))
  (setenv "EMACS_SOCKET_NAME" socket-path))

;; setenv wrapper that works for tramp remote shell as well
(defun settermenv (key val)
  ;; for a local env, it's simple as that
  (setenv key val)
  ;; for (remote) env for tramp
  (if (boundp 'tramp-remote-process-environment)
      (if (eq val nil)
          ;; setting `nil' is more complicated but can be done via finding the item and remove it
          (setq tramp-remote-process-environment
                (cl-remove-if
                 (lambda (elt) (string-match-p (concat "^" key "=") elt))
                 tramp-remote-process-environment))
        ;; setting an actual value is simpler
        (add-to-list 'tramp-remote-process-environment (concat key "=" val)))))

;; extension point for backend-specific setup that has to run on every
;; `prep-env-for-term' call but doesn't belong in this terminal-agnostic
;; function itself (e.g. vterm's `vterm-shell' tramp workaround, hooked in
;; from ./vterm.el) - stays empty (a no-op) for backends with nothing to add
(defvar term-enhance/prep-env-hook nil)

;; this is a function to let to prepare my (zsh) shell work well within emacs
;; needed to be called before a terminal launches
;; to cover all possible scenario it's currently being called in multiple places
;;
;; handing a NEW per-session var to children here? also add it to
;; ../../../shell/env-vars-to-exclude's FOR_DOOM_SYNC_ENV section, or
;; 'doom sync' run from a shell inside an editor bakes that session's
;; value into every future emacs launch
(defun prep-env-for-term ()
  (run-hooks 'term-enhance/prep-env-hook)
  ;; plain setenv, NOT settermenv: emacsclient against our socket is
  ;; local-only, a tramp remote must keep its own default (zshrc only
  ;; defaults EDITOR when unset, so this inherited value survives the
  ;; shell's own init - nvim hands out `nvimclient' the same way,
  ;; boot/misc.lua)
  (setenv "EDITOR" "emacsclient")
  (if tui-emacs (settermenv "TUI_EMACS" "1"))
  ;; desired default
  (settermenv "INSIDE_DOOM_EMACS" "1")
  (settermenv "UNSET_ALL_MY_ZSH_STUFF_LOADED" "1")
  (settermenv "UNSET_MY_BASIC_ZSH_STUFF_LOADED" "1")
  (settermenv "UNSET_HOST_ALWAYS_USE_TMUX" "1")
  ;; if THIS emacs runs inside nvim's :terminal, its markers would leak
  ;; into our terminals and pull in the wrong shell integration - scrub
  ;; them, shells in our terminals are emacs's ("nearest editor wins" in
  ;; ../../../../../docs/philosophy.md; nvim's boot/misc.lua scrubs the
  ;; emacs markers symmetrically)
  (settermenv "NVIM" nil)
  (settermenv "NVIM_LISTEN_ADDRESS" nil)
  (settermenv "VIMRUNTIME" nil))
;; invoke on startup so the local shell is ready
(prep-env-for-term)
;; invoke again for when tramp is ready
(after! tramp-sh (prep-env-for-term))
;; shared guts of "open the current file in a quick nvim, launched from
;; inside the terminal backend" - ./ghostel.el and ./vterm.el each only
;; supply the terminal-specific part (which `*-with-cmd' to call, and
;; vterm's extra TERM= prefix)
(defun term-enhance/prep-env-for-quick-editor ()
  (settermenv "ALL_MY_ZSH_STUFF_LOADED" "1")
  (settermenv "fast_shell_in_editor" "1")
  (settermenv "IGNORE_UNSET_ALL_MY_ZSH_STUFF_LOADED" "1"))

(defun term-enhance/undo-env-for-quick-editor ()
  (settermenv "ALL_MY_ZSH_STUFF_LOADED" nil)
  (settermenv "fast_shell_in_editor" nil)
  (settermenv "IGNORE_UNSET_ALL_MY_ZSH_STUFF_LOADED" nil))
