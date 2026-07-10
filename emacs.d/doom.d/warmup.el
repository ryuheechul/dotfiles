;;; warmup.el --- warm doom's deferred layers without blocking input -*- lexical-binding: t; -*-

;; the first REAL action after startup (e.g. the dashboard's "Recently
;; opened files", ~1.25s measured cold in GUI) would otherwise pay ALL of
;; doom's deferred layers at once - doom defers whole swaths of config to
;; the first input/buffer/file. Idle time is free time: warm everything
;; up front in small steps, drained only while the user is actually idle.
;;
;; every step runs to COMPLETION - deliberately no `while-no-input'
;; (doom's incremental loader uses it, and was used here at first):
;; aborting a `require' mid-file leaves the package half-evaluated, and
;; the very keypress that aborted it is also emacs' "first input" - doom's
;; transient trigger then runs hook functions against the half-loaded
;; package (seen live: marginalia-mode failing with (setting-constant
;; nil)). Bounded small blocks are sound; aborts are not. Worst single
;; step measured ~260ms (loading flycheck), only ever after the idle gate.
;;
;; everything lives in one lexical closure: once the finisher's timer
;; cancels itself, the whole stage - names, step queue, timer - becomes
;; garbage. Nothing is interned, nothing outlives the warmup. (the cost:
;; no live introspection or re-tuning; tweak here and reload instead)

(let ((packages
       ;; what to warm - every entry measured contributing to the stall.
       ;; consult must be listed: it loads on its first COMMAND, not on
       ;; any doom hook - the dashboard menu's recentf-open-files is
       ;; remapped to consult-recent-file (:completion vertico's
       ;; `[remap ...]'). tramp too: one remote path in recentf makes the
       ;; pick's candidate/icon handling autoload all of tramp (~200ms,
       ;; profiled). Ordered cheap-to-expensive (measured): a keypress
       ;; only ever waits on the step in flight, so the early window -
       ;; when colliding with the user is most likely - holds only
       ;; ~10-30ms steps, and the two heavyweights (tramp ~200ms,
       ;; flycheck ~260ms) come last
       '(nerd-icons nerd-icons-completion cape marginalia ws-butler
         vertico corfu orderless consult embark tramp flycheck))
      (first-tick-delay 0.25) ; let startup settle first
      (tick-interval 0.05)
      ;; only work when the user has been idle this long - together with
      ;; the budget below this keeps warmup work out of typing gaps
      ;; (a step that started right before a keypress still finishes,
      ;; that's the bounded-block trade described in the header)
      (idle-gate 0.5)
      ;; soft cap of work per tick: drain stops after the step that
      ;; crosses it, so a tick blocks at most budget + one worst step
      (tick-budget 0.04)
      (steps nil)
      (timer nil))
  (cl-labels
      (;; one step per function of doom's three transient first-hooks.
       ;; first-input carries vertico/marginalia/corfu mode toggles
       ;; (orderless has no hook fn - it loads via :after-call, covered
       ;; by the package list);
       ;; first-buffer carries flycheck & co, which otherwise fires
       ;; during the pick's first minibuffer REDISPLAY (it counts as the
       ;; first buffer switch, ~300ms of the stall); first-file is what
       ;; opening the picked file would then pay. Plain funcalls of what
       ;; `run-hooks' would have called - doom's own transient trigger
       ;; may race us on real input, which is fine: mode toggles are
       ;; idempotent, and the packages behind them are fully loaded by
       ;; the time these steps exist
       (hook-steps ()
         (mapcan (lambda (h)
                   (mapcar (lambda (fn)
                             (lambda () (ignore-errors (funcall fn))))
                           (seq-filter #'functionp (symbol-value h))))
                 '(doom-first-input-hook
                   doom-first-buffer-hook
                   doom-first-file-hook)))

       ;; one step per recentf entry, priming nerd-icons' memoization.
       ;; nerd-icons caches per EXACT argument list, so each step makes
       ;; the very call the completion UI will make (abbreviated name,
       ;; file category) - a bare `nerd-icons-icon-for-file' on the raw
       ;; path would cache under a key the recent-files pick never looks
       ;; up
       (icon-steps ()
         (mapcar (lambda (f)
                   (lambda ()
                     (nerd-icons-completion-get-icon
                      (abbreviate-file-name f) 'file)))
                 (bound-and-true-p recentf-list)))

       ;; drain steps while idle, within the budget; self-cancel when done
       (finish-tick ()
         (cond
          ((null steps) (cancel-timer timer))
          ((let ((idle (current-idle-time)))
             (and idle (> (float-time idle) idle-gate)))
           (let ((deadline (+ (float-time) tick-budget)))
             ;; input-pending-p: the user's keypress ends the drain right
             ;; after the in-flight step - without it a burst of ticks
             ;; (e.g. the very first seconds, all idle) keeps stacking
             ;; hitches against someone who just started typing
             (while (and steps
                         (< (float-time) deadline)
                         (not (input-pending-p)))
               (funcall (pop steps))))))))

    (setq steps
          (append
           ;; 1. load the packages, one full require per step
           (mapcar (lambda (p) (lambda () (require p nil t))) packages)
           ;; 2. then the hook steps - built lazily, AFTER the requires
           ;;    ran, so they see the final hook contents
           (list
            (lambda ()
              (setq steps
                    (append
                     (hook-steps)
                     ;; 3. and the icon steps lazier still: the
                     ;;    first-file hook step may run recentf-load-list
                     ;;    and replace recentf-list - snapshotting the
                     ;;    candidates any earlier would warm a list the
                     ;;    pick no longer renders
                     (list (lambda ()
                             (setq steps (append (icon-steps) steps))))
                     steps)))))
          ;; (run-with-timer SECS REPEAT FN: first fire after SECS
          ;; seconds, then every REPEAT). Wall-clock repeat, NOT a
          ;; repeating idle timer - that fires only once per idle
          ;; STRETCH, so a drain paused by the idle gate would never
          ;; resume until input arrives
          timer (run-with-timer first-tick-delay tick-interval
                                #'finish-tick))))
