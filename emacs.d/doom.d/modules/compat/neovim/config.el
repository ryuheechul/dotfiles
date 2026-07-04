;;; compat/neovim/config.el -*- lexical-binding: t; -*-

;; This layer exist to reduce the gap of neovim's default behavior + my config at =../../../../../nvim/=
;; There is also =../../my-custom/morevil/= that is more concerned on muscle memory (via keybindings);
;; when this module is more concerned in a bigger system layer

;; fuzzy match completions!!!
(after! orderless ;; orderless is cool and all but I'm used to fuzzy matching more ...
  (defun hotfuzz-setup ()
    "Set up `hotfuzz'."
    (setq completion-ignore-case t)
    (unless (memq 'hotfuzz completion-styles)
      (push 'hotfuzz completion-styles)))

  ;; to overtake orderless set by :completion vertico
  (use-package! hotfuzz
    :config
    (hotfuzz-setup)))

;; to make this to resemble the completion experience from Neovim
(after! corfu
  ;; let's not make the enter key embarassing when there is no candidate
  (setq corfu-preselect 'first))

;; let the cursor move past the end of a line - PARTIAL parity with
;; ../../../../../nvim/lua/boot/misc.lua's `vim.o.virtualedit = 'all'`.
;; this only gets `virtualedit=onemore' (one extra column, a real buffer
;; position before the newline) not `all' (floating in space with no
;; backing character) - emacs' `point' can't represent the latter at all,
;; it's always a position between two actual characters. genuinely
;; unsolved upstream, not evil-mode being lazy: see open feature request
;; https://lists.nongnu.org/archive/html/bug-gnu-emacs/2025-04/msg00088.html
;; (bug#77438). living with one-column for now.
(setq evil-move-beyond-eol t)

;; show hidden/whitespace characters, same as
;; ../../../../../nvim/lua/boot/filetype.lua's `vim.opt.list' + `listchars'
(use-package! whitespace
  :hook (doom-first-buffer . global-whitespace-mode)
  :config
  (setq whitespace-style '(face tabs tab-mark trailing newline newline-mark))
  (setq whitespace-display-mappings
        '((tab-mark ?\t [?> ?\t] [?\\ ?\t])
          (newline-mark ?\n [?↵ ?\n] [?$ ?\n]))))
;; two things filetype.lua's listchars does that have no port here:
;; - `trail:·' marks trailing whitespace with a literal character; whitespace-mode has
;;   no per-trailing glyph substitution (only `space-mark', which would mark ALL spaces,
;;   not just trailing ones) - `trailing' above highlights it via face instead, same
;;   information conveyed, different visual treatment (background face, not a glyph)
;; - `extends'/`precedes' (line-wrap continuation arrows, only visible when nowrap) need
;;   nothing here - emacs already shows the same thing natively via fringe arrows
;;   whenever `truncate-lines' is set on a buffer

;; make `q' mimic neovim's overall quit semantics - see ./smart-quit.el
(load! "smart-quit")
