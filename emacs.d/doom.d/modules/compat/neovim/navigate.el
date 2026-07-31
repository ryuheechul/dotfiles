;;; compat/neovim/navigate.el -*- lexical-binding: t; -*-

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

;; `evil-respect-visual-line-mode' is not used (see ./init.el) because it
;; installs visual-line-aware bindings in the MOTION state, which leaks into
;; operator-pending and visual via `:enable' and changes internal functions
;; (evil-line-or-visual-line, evil-expand-line-for-line-based-operators) to
;; operate on screen lines - breaking dd, yy, V, D, etc.  nvim limits its
;; wrap-aware remapping to NORMAL mode only (j->gj, etc.), leaving all other
;; contexts (operator-pending, visual, line-based operators) on logical lines.
;; Replicate that narrow scope here.
(after! evil
  ;; Normal state: visual-line-aware j/k/0/$ (nvim's gj/gk/g0/g$)
  (evil-define-minor-mode-key 'normal 'visual-line-mode
    "j" #'evil-next-visual-line
    "k" #'evil-previous-visual-line
    "0" #'evil-beginning-of-visual-line
    "$" #'evil-end-of-visual-line)

  ;; Inherited by operator-pending and visual via `:enable', so override
  ;; back to logical lines there (nvim: j = logical in these states).
  (dolist (state '(operator visual))
    (evil-define-minor-mode-key state 'visual-line-mode
      "j" #'evil-next-line
      "k" #'evil-previous-line
      "0" #'evil-beginning-of-line
      "$" #'evil-end-of-line)))

;; f/t/F/T need no wrap handling here: doom's evil-snipe override (scope
;; 'line) searches the whole logical line past the wrap fold, matching
;; nvim's f/t (flit.nvim there) - and it never consults
;; evil-respect-visual-line-mode, which must stay nil (see init.el)

;; nvim's C-] (bound to gl there, "go to the link under the cursor") is
;; first and foremost the :help navigation key - it follows the help-tag
;; HYPERLINK under the cursor; lsp's tagfunc merely extended the same key
;; to code, where it means "first match right away, never a list". that
;; is unlike gd (vim.lsp.buf.definition), which offers a selection when
;; the server reports several entities (lua-ls: name + body) - so gd's
;; picker is correct parity and this is gl's no-picker counterpart
;; (bound in ../../my-custom/morevil/)
(defun +neovim/goto-link ()
  "Follow the link at point like nvim's C-].
A real link (button, Info reference) when on one - the :help heritage -
otherwise the first definition, tag-jump style, no picker."
  (interactive)
  (cond
   ;; help-mode xref links, custom links, ... are all buttons
   ((button-at (point)) (push-button))
   ((derived-mode-p 'Info-mode) (Info-follow-nearest-node))
   ((let* ((backend (xref-find-backend))
           (id (xref-backend-identifier-at-point backend))
           (defs (and id (xref-backend-definitions backend id))))
      (when defs
        ;; record the jump so nvim-style go/gn can come back
        (evil-set-jump)
        (xref-pop-to-location (car defs))
        t)))
   (t (user-error "No link to follow at point"))))

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
