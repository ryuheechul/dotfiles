;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       ;; company           ; the ultimate code completion backend
       (corfu +icons)    ; complete with cap(f), cape and a flying feather!
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;;ivy               ; a search engine for love and life
       (vertico +icons)  ; the search engine of the future

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;indent-guides     ; highlighted indent columns
       ;;ligatures         ; ligatures and symbols to make your code pretty again
       ;;minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;tabs              ; a tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       ;;;; manually enabling at ./config.el until this issue is resolved - https://github.com/doomemacs/doomemacs/issues/8277
       ;; (vc-gutter +pretty) ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       ;;window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       ;; this actually gives worse experience like mis matching the buffer and file and doesn't get fixed over time
       ;; (format +onsave +lsp) ; automated prettiness
       ;; the below on the other hand, still work with lsp even with `+lsp' flag.
       (format +onsave) ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;;; avoid lispy until there is a good solution to this issue (conflict with evil-escape), https://github.com/abo-abo/lispy/issues/635
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ;;eww               ; the internet is gross
       ;;ibuffer           ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       (spell +flyspell) ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;biblio            ; Writes a PhD for you (citation needed)
       ;;collab            ; buffers with friends
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       direnv            ; bring direnv to emacs - helpful for seamless org babel code executions
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       (lsp +eglot)       ; M-x vscode
       (lsp-support +hover); ./modules/tools/lsp-support/
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       tramp-support     ; ./modules/tools/tramp-support/
       ;;tree-sitter       ; syntax and parsing, sitting in a tree...
       treesit-support   ; ./modules/tools/treesit-support/
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if (featurep :system 'macos) macos)  ; improve compatibility with macOS
       (tty +osc)        ; improve the terminal Emacs experience

       :lang
       (astro +lsp)        ; ./modules/lang/astro/
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       ;;(cc +lsp)         ; C > C++ == 1
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;dhall
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;(graphql +lsp)    ; Give queries a REST
       ;;(haskell +lsp)    ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       ;;json              ; At least it ain't XML
       ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
       (javascript +lsp) ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean              ; for folks with too much to prove
       ;;ledger            ; be audit you can be
       lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       (nix +lsp)        ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org +pretty)     ; organize your plain life in plain text
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;graphviz          ; diagrams for confusing yourself even more
       ;;purescript        ; javascript, but functional
       ;;python            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;(rust +lsp)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       (svelte +lsp)     ; ./modules/lang/svelte/config.el
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes
       ;;yaml              ; JSON, but readable
       ;;zig               ; C, but simpler

       :ext-lang
       (nix +lsp)         ; ./modules/ext-lang/nix/
       (lua +lsp)         ; ./modules/ext-lang/lua/
       typescript         ; ./modules/ext-lang/typescript/

       :email
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader

       :config
       ;;literate
       (default +bindings +smartparens)

       :compat
       neovim              ; ./modules/compat/neovim

       ;; from here and below are my more obvious private modules
       :my-custom          ; my own modules
       morevil             ; when there is more dark power needed
       vterm-enhance       ; bring tighter integration with my existing tools
       soom                ; when doomemacs does not agree with me
       org                 ; addtional stuff for org mode
       lab)                ; where my new configs will be added initially

;; set it early here instead of ./config.el to avoid seeing frame changes on start up
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; for the time being - https://www.reddit.com/r/emacs/comments/oza47b/doom_emacs_i_get_the_vanilla_emacs_splash_screen/
;; this makes the period of "flash of unstyled emacs" shorter but still there
(setq inhibit-x-resources t)
(setq-default inhibit-redisplay t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil)
            (redisplay))
          100)
;; end of for the time being

;;;; this is essentially for when =doom env=/=doom sync= running
;; this section is for cli usages https://github.com/doomemacs/doomemacs/issues/2434#issuecomment-629572501
(when noninteractive
  ;; ignore terminal and tmux related env vars
  ;; define if not already exist - https://stackoverflow.com/a/7301151
  (defvar doom-env-deny '()) ;; previously it was =doom-env-blacklist= - https://github.com/doomemacs/doomemacs/issues/2434
  ;; (mapc ... will result in the same effect as these example lines
  ;; (add-to-list 'doom-env-deny "^TERM$")
  ;; (add-to-list 'doom-env-deny "^TERM_PROGRAM$")
  ;; ...
  (mapc (lambda (x) (add-to-list 'doom-env-deny x))
        (mapcar (lambda (x) (format "^%s$" x))
                (string-lines
                 (shell-command-to-string
                  (concat
                   "FOR_DOOM_SYNC_ENV=1 "
                   doom-private-dir
                   "shell/env-vars-to-exclude"))))))
