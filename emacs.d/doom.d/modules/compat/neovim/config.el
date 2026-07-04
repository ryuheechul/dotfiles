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

;; nvim's dashboard has an official menu entry for opening a fresh
;; unnamed buffer (quick scratch note, usually never saved; entering
;; insert state is up to the user afterwards, in nvim too) - give doom's
;; dashboard the same, as a real menu item rather than a hidden
;; keybinding. doom's +default/new-buffer already does exactly this.
(add-to-list '+dashboard-menu-sections
             '("New unnamed buffer"
               :icon (nerd-icons-faicon "nf-fa-pencil" :face '+dashboard-menu-title)
               :action +default/new-buffer)
             'append)

;; the binding must live in the evil NORMAL auxiliary keymap of
;; +dashboard-mode-map (what map!'s :n does here) for two reasons: that
;; map is where the menu renderer looks up the key hint it displays next
;; to the entry (+dashboard--insert-menu), and a direct binding there
;; preempts the module's own `[remap evil-insert] -> ignore', which
;; otherwise swallows i before any remap in the raw keymap is consulted
(map! :map +dashboard-mode-map :n "i" #'+default/new-buffer)

;; never let the dashboard itself enter insert state - it is a read-only
;; menu, there is nothing to insert into. an :around advice, not an entry
;; hook: switching states from inside evil's entry hook loses to the
;; in-flight transition, refusing to start it at all cannot
(defadvice! +neovim/dashboard-block-insert-a (fn &rest args)
  "Refuse to enter insert state in the dashboard."
  :around #'evil-insert-state
  (unless (derived-mode-p '+dashboard-mode)
    (apply fn args)))
