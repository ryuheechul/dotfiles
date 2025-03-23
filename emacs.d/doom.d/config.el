;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; NOTE refrain adding too much code directly here
;; - there is ./modules/ that I can split code
;;   - and don't forget to add that module to ./init.el and run =doom sync= or just run =(doom/reload)=
;;   - (doom/help-modules) to index and access modules
;;   - (doom/help-pacakges) to look up packages
;; - try using =(doom/reload)= to see if applying the new changes can be faster without much delay such as restarting
;;   - but even before that just running to code at the cursor with `eval-last-sexp'
;;     - (place cursor at the end of the expression, like ="= or =)=)
;;   - there is also =(doom/restart)= when reload is not good enough
;;   - actually more methods are documented here,  https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org#reloading-your-config
;; - there is also my note, [[https://gist.github.com/ryuheechul/1363eec4e945b6840989d255cdbd09e3][emacs.md]]
;; - https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org
;; - https://github.com/doomemacs/doomemacs/blob/master/docs/faq.org
;; NOTE make sure to run =doom env= if to match =$SSH_AUTH_SOCK= from my shell

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-solarized-light)
(setq doom-font
      ;; provide a way to favor the font size value from env var
      ;; NOTE (to be able to get that value) make sure to run =doom env= if your shell has set MY_EMACS_FONT_SIZE if there is any trouble laoding the env var
      (let ((size (string-to-number (or (getenv "MY_EMACS_FONT_SIZE") "12")))
            (family "FiraMono Nerd Font Mono"))
        (font-spec :family family :size size))
      ;; ;; There are several things I did to make fonts work "properly" (tested on GUI Emacs on macOS).
      ;; ;; Prior to these patches, because of the (undesired) differences that I saw from Emacs compare to terminal,
      ;; ;; I fulled myself to believe that maybe it was the limitations on any of these:
      ;; ;; - eterm-color (TERMINFO)
      ;; ;; - https://github.com/akermu/emacs-libvterm
      ;; ;; - Emacs
      ;; ;; Now I know that was not the case.
      ;;
      ;; 1. use `FiraMono Nerd Font` instead of `JetBrainsMono Nerd Font`
      ;;    because somehow the height of glyphs can vary with JetBrainsMono even though they both come from Nerd Fonts.
      ;;    (I don't have this issue with terminals that I use - Alacritty, iTerm, etc.)
      ;; 2. enabling `unicode` module under :ui at ./init.el would fix misalignments on width in `vterm` (and probably others)
      ;;    due to non-monospace (width) unicode fonts.
      ;; 3. enabling `emoji` module under :ui at ./init.el would fix misalignments on height in emacs not just with `[v]term`
      ;;    due to non-monospace (height) native emoji font like Apple Color Emoji.
      ;; 4. adding the line below to use the font that I assign for glyphs instead of whatever the default that doom-emacs comes with for mono width.
      ;; 5. nerd-fonts release fonts sometimes Fira ... and Fura ... and not finding the exact name will freeze/break GUI emacs UI
      ;; - https://github.com/ryanoasis/nerd-fonts/blame/master/bin/scripts/lib/fonts.json
      ;; - https://github.com/ryanoasis/nerd-fonts/commit/72b9ec663bb16e7e77c56d6061aed8f5452cb0de
      ;; - https://github.com/ryanoasis/nerd-fonts/commit/8b3257d6766f40c98289c6193209e0700a4cc3d0
      doom-unicode-font doom-font)

;; remapping https://github.com/doomemacs/doomemacs/blob/master/modules/config/default/%2Bevil-bindings.el for better zoom experience;
;; by stealing the shortcut from 'text-scale-[increase|decrease]'
(after! evil
  (map! :n "C-=" #'doom/increase-font-size
        :n "C--" #'doom/decrease-font-size))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; now this is done at ./init.el
;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))
;; above now replaces the commented one below

;; (when (or IS-MAC IS-LINUX)
;;   ;; see for "flash of unstyled Emacs" - https://www.reddit.com/r/emacs/comments/oza47b/comment/h7yfxjz
;;   ;; also it's possible that maybe somehow *skipping* of running `doom install` might have caused this.
;;   ;; if it's a very first time running the GUI that gets stuck or hangs, running `emacsclient` first might fix it.
;;   ;;
;;   ;; to trigger emacs to be full screen on start up - https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/
;;   (add-hook 'window-setup-hook #'toggle-frame-fullscreen))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
