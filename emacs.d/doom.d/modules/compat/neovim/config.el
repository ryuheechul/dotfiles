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

;; ./ and ../ path completion everywhere, like nvim's cmp-path source
;; (../../../../../nvim/lua/plugins/config/completion.lua lists it
;; globally): doom's corfu module wires `cape-file' into prog-mode only -
;; extend it to the text/conf families (org/markdown derive from
;; text-mode, so they're covered)
(add-hook! '(text-mode-hook conf-mode-hook) #'+corfu-add-cape-file-h)

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
          (newline-mark ?\n [?↵ ?\n] [?$ ?\n])))
  ;; :editor whitespace's own per-buffer indent-mismatch highlighter already
  ;; no-ops itself whenever `global-whitespace-mode' is on (see its own guard
  ;; clause), which it always is here - remove it explicitly so that bypass
  ;; is documented instead of incidental
  (remove-hook 'after-change-major-mode-hook
               #'+whitespace-highlight-incorrect-indentation-h))

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

;; nvim's `autoread` (+ the autoread/shortmess tweak in boot/misc.lua):
;; a file changed outside emacs (git checkout, another editor, a
;; formatter...) reloads automatically instead of asking "file changed on
;; disk, really edit?" later; non-file buffers like dired refresh too
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; same idea, the other direction: visit a file from a magit hunk (`e'),
;; save, `q' back - the status buffer should already show the new diff,
;; not the stale one from before the edit. global-auto-revert-non-file-
;; buffers above is timer/focus-polled, not immediate, and doesn't
;; reliably catch this; magit's own magit-after-save-refresh-status
;; (intended for after-save-hook, not wired by default) refreshes right
;; on save instead
(add-hook 'after-save-hook #'magit-after-save-refresh-status)

;; restore the last cursor position when reopening a file (nvim: the
;; LastPlace autocmd in boot/misc.lua) - measured OFF in this doom
;; checkout, so enable it here. saveplace's default
;; `save-place-ignore-files-regexp' already excludes COMMIT_EDITMSG,
;; matching the gitcommit exclusion the nvim autocmd makes
(save-place-mode 1)

;; rainbow bracket depth highlighting, like nvim's
;; hiphish/rainbow-delimiters.nvim (extra.lua) - doom has no bundled module
;; for this. scoped to prog-mode, same convention as flyspell-prog-mode
;; below; nvim's custom highlight-group/color order not ported, doom
;; themes already style rainbow-delimiters-depth-N-face out of the box
(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; syntax-highlighted magit hunks, like nvim's diffview.nvim/treesitter
;; diffs - magit's own diff rendering has no language syntax highlighting
;; at all (only add/remove line coloring), nothing to configure away.
;; `delta' (the binary) is already installed and wired into ../../../../../gitconfig
;; for CLI git diff/pager - magit-delta just pipes magit's diff buffers
;; through that same binary for the identical look
(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode)
  :config
  ;; bug (2026-07-11): staging/unstaging a hunk errored "corrupt patch".
  ;; ../../../../../gitconfig's [delta] features = line-numbers decorations
  ;; leaks into every delta call incl. magit-delta's - `--color-only'
  ;; only promises not to reorder diff content, not to suppress that.
  ;; the line-number gutter (e.g. "  1 ⋮  1 │ ") is real text, so it
  ;; ends up prefixed on every diff line magit later extracts to build
  ;; the patch it feeds `git apply' - not valid unified-diff. override
  ;; features for magit-delta's own calls only; CLI git diff keeps
  ;; line numbers, untouched
  (setq magit-delta-delta-args
        (append magit-delta-delta-args '("--features" ""))))

;; nvim has global `spell`; doom's :checkers spell already covers prose
;; modes (org, markdown, ...) - extend to code via flyspell-prog-mode,
;; whose narrow scope keeps the overhead negligible
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
;; ... and match nvim's scope while at it: its treesitter @spell captures
;; check comments, NOT plain strings (identifiers/paths/format specifiers
;; there are all false positives) - flyspell-prog-mode does strings by
;; default, so drop that; docstrings stay, they are prose
(after! flyspell
  (setq flyspell-prog-text-faces
        (remq 'font-lock-string-face flyspell-prog-text-faces))
  ;; eglot semantic tokens turn the `face' text property into a LIST
  ;; (e.g. (eglot-semantic-comment font-lock-comment-face)) and the stock
  ;; predicate's bare memq cannot see into it - flyspell then silently
  ;; skips every semantically-fontified comment word (typos only got
  ;; flagged when the check won the race against the async tokens)
  (defadvice! +neovim/flyspell-prog-verify-face-lists-a ()
    :override #'flyspell-generic-progmode-verify
    (unless (eql (point) (point-min))
      (let ((f (get-text-property (1- (point)) 'face)))
        (cl-intersection (ensure-list f) flyspell-prog-text-faces))))
  ;; the stock faces hide behind a `(supports :underline (:style wave))'
  ;; display guard, so on terminals that don't advertise styled underlines
  ;; they render as NOTHING at all (invisible flags). replace with an
  ;; unconditional wave that leaves the text color alone - a hint, not an
  ;; alarm - degrading to a plain underline (never to nothing) where the
  ;; terminal can't style underlines. yellow3/cyan3 are the exact RGB the
  ;; terminal palette renders "yellow"/"cyan" as, so GUI and TUI frames
  ;; come out identical; cyan for duplicates over the stock DarkOrange
  ;; because orange blurs into the yellow of misspellings.
  ;; face-OVERRIDE-spec, not defface spec: doom-themes define these faces
  ;; too and a theme beats a defface, but nothing beats the override spec
  (face-spec-set 'flyspell-incorrect
                 '((t :underline (:style wave :color "yellow3")))
                 'face-override-spec)
  (face-spec-set 'flyspell-duplicate
                 '((t :underline (:style wave :color "cyan3")))
                 'face-override-spec))

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

;; nvim-parity clipboard PASTE for TTY frames: `:os (tty +osc)' already
;; sends kills OUT via OSC 52, but OSC 52 is write-only in practice
;; (terminals refuse clipboard reads - e.g. ghostty defaults
;; clipboard-read to ask), so evil's p in a terminal frame never saw the
;; system clipboard that GUI frames read natively. Same layering rule as
;; nvim's provider pick (../../../../../nvim/lua/boot/misc.lua): prefer
;; the immediate native provider when it's there - GUI frames keep the
;; native path, TTY frames read pbpaste. That's usually the stack-aware
;; wrapper (../../../../../bin/path/default/pbpaste: native mac / osc /
;; xsel), so remote frames work too wherever the wrapper does; no
;; pbpaste at all = copy-only, like nvim under zellij ("One clipboard,
;; every layer" in ../../../../../docs/philosophy.md). Deduping against
;; the kill-ring head is sound because +osc keeps the system clipboard
;; in step with our own kills - a fresh internal kill is already ON the
;; clipboard, so it never gets shadowed by a stale read.
(defvar +neovim/--gui-paste-function interprogram-paste-function
  "The stock (GUI) paste function this module wraps.")

(defun +neovim/frame-aware-paste ()
  "Read the system clipboard appropriately for the selected frame."
  (if (display-graphic-p)
      (and +neovim/--gui-paste-function
           (funcall +neovim/--gui-paste-function))
    (when (executable-find "pbpaste")
      (let ((text (shell-command-to-string "pbpaste")))
        (unless (or (string-empty-p text)
                    (equal text (car kill-ring)))
          text)))))
(setq interprogram-paste-function #'+neovim/frame-aware-paste)
