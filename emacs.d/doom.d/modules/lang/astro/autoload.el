;;; lang/astro/autoload.el -*- lexical-binding: t; -*-

;; astro-ts-mode's autoloads call `treesit-ready-p' at load time without
;; requiring `treesit' first. That's fine during `doom sync' (packages.el
;; already required it in-process by then), but at real startup Doom loads
;; a pre-generated file that just inlines every package's autoloads
;; verbatim, without packages.el ever running. Module autoload.el files
;; are scanned into that generated init at a lower priority than package
;; autoloads, so requiring it here guarantees it's loaded first.
;;;###autoload
(require 'treesit)
