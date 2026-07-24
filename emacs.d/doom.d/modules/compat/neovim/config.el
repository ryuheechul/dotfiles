;;; compat/neovim/config.el -*- lexical-binding: t; -*-

;; This layer exist to reduce the gap of neovim's default behavior + my config at =../../../../../nvim/=
;; There is also =../../my-custom/morevil/= that is more concerned on muscle memory (via keybindings);
;; when this module is more concerned in a bigger system layer

;; Completion: fuzzy match, corfu TAB, path/buffer completion
(load! "completion")
;; Navigate: cursor movement, link following, dashboard menu
(load! "navigate")
;; Visual aids: whitespace, fill-column indicator, rainbow brackets, magit-delta
(load! "visual-aid")
;; make `q' mimic neovim's overall quit semantics - see ./smart-quit.el
(load! "smart-quit")
;; Sync: auto-revert, magit refresh, save-place
(load! "sync")
;; Spell checking in code
(load! "spell")
;; Clipboard paste for TTY frames
(load! "clipboard")
