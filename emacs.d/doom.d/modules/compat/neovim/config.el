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

;; nvim's TAB completes a lone match (nvim-cmp, ../../../../../nvim/lua/plugins/config/completion.lua);
;; doom binds corfu's TAB to `corfu-next' (config/default's `cmds-tab'), which
;; with `corfu-preselect 'first' + `corfu-cycle t' just re-selects the only
;; candidate - looks like a no-op. cycle when there are several, insert when
;; there is one. a lone matched DIRECTORY still keeps offering its contents
;; like nvim's path completion, but that continuation is now driven by the
;; cape-file exit-function below (which also handles RET), so a plain
;; `corfu-insert' is enough here - no need for `corfu-complete'.
;;
;; the fresh-popup branch fixes a "TAB skips the first candidate" bug (most
;; visible on ./ path completion): `corfu-preselect 'first' silently selects
;; candidate 0, but corfu previews a candidate only when point differs from
;; the preselect (corfu--preview-current-p), so candidate 0 shows as
;; unselected/unfilled - and a plain `corfu-next' then jumps straight to
;; candidate 1, skipping 0. nvim's completion.lua uses `menuone,noselect', so
;; the first TAB lands ON the first candidate; match that by detaching the
;; preselect on that first press so candidate 0 becomes previewable. RET keeps
;; inserting candidate 0 (still index >= 0, so no "embarrassing newline"), and
;; later presses cycle normally.
(defun +neovim/corfu-tab ()
  "TAB in the corfu popup: cycle when several candidates, complete a lone one.
On a fresh popup, land on and preview the first candidate instead of skipping
it (see the block comment above)."
  (interactive)
  (cond
   ((= corfu--total 1) (corfu-insert))
   ;; reaches into corfu internals (corfu--index/--preselect/--goto): the
   ;; public API has no "preview the preselected candidate" lever.
   ((and (>= corfu--index 0) (= corfu--index corfu--preselect))
    (setq corfu--preselect -1)
    (corfu--goto 0))
   (t (corfu-next))))

;; to make this to resemble the completion experience from Neovim
(after! corfu
  ;; let's not make the enter key embarassing when there is no candidate
  (setq corfu-preselect 'first)
  ;; bind BOTH key symbols - "TAB" (TTY) and [tab] (GUI), as doom does - this
  ;; module has been bitten by GUI/TTY key-symbol splits before. runs on corfu
  ;; load, after doom's config/default `cmds-tab' binding, so it wins.
  (map! :map corfu-map
        :gi "TAB" #'+neovim/corfu-tab
        :gi [tab] #'+neovim/corfu-tab)
  ;; corfu quits after any command not in `corfu-continue-commands' (nor
  ;; matching its built-in `\\`corfu-' regex) - our wrapper name matches
  ;; neither, so without this the popup dies after a single press: it cycles
  ;; once, then `corfu--post-command' calls `corfu-quit'. doom registers its
  ;; own +corfu/* wrappers here for exactly this reason.
  (add-to-list 'corfu-continue-commands #'+neovim/corfu-tab))

;; ./ and ../ path completion everywhere, like nvim's cmp-path source
;; (../../../../../nvim/lua/plugins/config/completion.lua lists it
;; globally): doom's corfu module wires `cape-file' into prog-mode only -
;; extend it to the text/conf families (org/markdown derive from
;; text-mode, so they're covered)
(add-hook! '(text-mode-hook conf-mode-hook) #'+corfu-add-cape-file-h)

;; keep offering choices as you descend into a directory, like nvim's
;; cmp-path: cape-file ships no `:exit-function', so after you complete a
;; `dir/' corfu just stops and you have to re-invoke completion by hand.
;; inject an exit-function that re-triggers `completion-at-point' whenever the
;; inserted candidate ends in `/' (a directory) - fires for RET (a dir picked
;; from several) and for TAB (a lone dir, via `corfu-insert' above) alike, and
;; recurses naturally as you go deeper. guarded so a future cape-file that
;; grows its own exit-function wins.
(defun +neovim/cape-file-continue-in-dir-a (result)
  "Advice: make cape-file re-open completion after inserting a directory."
  (when (and (consp result) (number-or-marker-p (car-safe result)))
    (let ((tail (nthcdr 3 result)))
      (unless (plist-get tail :exit-function)
        (setf (nthcdr 3 result)
              (plist-put tail :exit-function
                         (lambda (str status)
                           (when (and (eq status 'finished)
                                      (string-suffix-p "/" str))
                             (completion-at-point))))))))
  result)
(advice-add #'cape-file :filter-return #'+neovim/cape-file-continue-in-dir-a)

;; nvim's `buffer' cmp source (completion.lua): complete words already in the
;; buffer (type "app" -> "apple"). doom gates this behind the corfu module's
;; +dabbrev flag, which isn't set here, so wire `cape-dabbrev' in directly.
;; depth 20 keeps it a low-priority fallback so real capfs (lsp, file) are
;; offered first, matching nvim listing `buffer' after lsp/path among its
;; sources. corfu-auto's 2-char prefix triggers it earlier than nvim's
;; keyword_length 4, which is the more responsive feel the buffer source is
;; wanted for anyway.
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  (defun +neovim/add-cape-dabbrev-h ()
    (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t)))

;; nvim-cmp shows every source at once; emacs' capf list is a CHAIN where the
;; first capf that returns matches wins, so in prose buffers the spell source
;; (`ispell-completion-at-point', text-mode only) shadows `cape-dabbrev' - type
;; "curr" next to a "current-base16" and you see only dictionary words until
;; the word is finished, then the buffer word finally appears. merge the two
;; with `cape-capf-super' so spelling AND buffer words show together like nvim.
;; added ahead (depth -1) of the standalone spell/dabbrev entries so it wins
;; and they never shadow it; `cape-file' (depth -10) still wins first for real
;; paths. text-mode only: that is where the spell capf runs (prog-mode's
;; flyspell only flags, conf-mode has no spell capf to shadow dabbrev).
;;
;; ranking buffer words ABOVE the dictionary needs a real sort override, not
;; just source order: the completion style (hotfuzz) sorts candidates by fuzzy
;; score, and cape-super's own `:display-sort-function' is `identity' (keeps
;; that score order), so a long buffer word like "current-base16" sinks below
;; the dozens of short dictionary words that "bury others". `corfu--sort-
;; function' consults `corfu-sort-override-function' FIRST (before any
;; display-sort-function), so set that: partition buffer words to the front.
(defun +neovim/corfu-buffer-words-first (cands)
  "Sort CANDS so buffer words (the dabbrev source) rank above the rest.
A candidate counts as a buffer word if it appears verbatim as a token in the
buffer; `[[:alnum:]_-]+' spans the hyphenated identifiers dabbrev completes
\(e.g. current-base16). Stable within each group, so the style's own ordering
survives; a single buffer scan keeps it cheap even for large candidate sets."
  (let ((words (make-hash-table :test #'equal))
        in out)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[[:alnum:]_-]+" nil t)
        (puthash (match-string-no-properties 0) t words)))
    (dolist (c cands)
      (if (gethash c words) (push c in) (push c out)))
    (nconc (nreverse in) (nreverse out))))

(add-hook! 'text-mode-hook :append
  (defun +neovim/merge-spell-and-buffer-completion-h ()
    (add-hook 'completion-at-point-functions
              (cape-capf-super #'cape-dabbrev #'ispell-completion-at-point)
              -1 t)
    (setq-local corfu-sort-override-function #'+neovim/corfu-buffer-words-first)))

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

;; keep dj/dk/yj/yk (and cj, >j, ...) linewise over FULL lines including the
;; cursor line, like nvim. `evil-respect-visual-line-mode' (./init.el, for the
;; visual-line j/k navigation) installs a visual-line-mode keymap in MOTION
;; state binding j/k to the exclusive `evil-next-visual-line' /
;; `evil-previous-visual-line' (evil-integration.el); operator-pending inherits
;; motion state, so `dj' turned into a 1-line exclusive delete that dropped the
;; line below - a real muscle-memory hazard (delete the wrong line). nvim
;; sidesteps this by remapping j->gj in normal mode only, not operator-pending;
;; mirror that by restoring the linewise motions in OPERATOR state, so
;; operators span whole lines again while bare j/k keep visual-line nav.
(after! evil
  (evil-define-minor-mode-key 'operator 'visual-line-mode
    "j" #'evil-next-line
    "k" #'evil-previous-line))

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
