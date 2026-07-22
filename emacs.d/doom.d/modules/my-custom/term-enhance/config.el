;;; $DOOMDIR/modules/my-custom/term-enhance/config.el -*- lexical-binding: t; -*-

;; terminal-agnostic plumbing shared by ./ghostel.el and ./vterm.el - nothing
;; here should ever need to know which backend is active. per-backend
;; specifics live in ./ghostel.el / ./vterm.el; ./integration.el decides
;; which of the two a shared keybinding actually calls.

(setq tui-emacs (eq window-system nil))

;; Shared terminal behavior is divided by responsibility. These load before
;; either backend so Ghostel and VTerm use the same child environment, editor
;; bridge, and pane operations.
(load! "environment")
(load! "editor-bridge")
(load! "mux")

;; Backend implementations and the adapter that selects between them.
(load! "theme")
(load! "ghostel")
(load! "vterm")
(load! "integration")
