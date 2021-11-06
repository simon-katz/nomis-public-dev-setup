;;;; Init stuff -- CIDER REPL history hacks --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; Hack `cider-repl--history-replace` so that if we go beyond the bounds of
;;;; history, the input is cleared. See `:nomis-hack` below.

(cond
 ((member (pkg-info-version-info 'cider)
          '("1.2.0snapshot (package: 20211105.708)"))

  (with-eval-after-load 'cider-repl
    ;; The original is in `cider-repl`.
    (defun cider-repl--history-replace (direction &optional regexp)
      "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list).
If REGEXP is non-nil, only lines matching REGEXP are considered."
      (setq cider-repl-history-pattern regexp)
      (let* ((min-pos -1)
             (max-pos (length cider-repl-input-history))
             (pos0 (cond ((cider-history-search-in-progress-p)
                          cider-repl-input-history-position)
                         (t min-pos)))
             (pos (cider-repl--position-in-history pos0 direction (or regexp "")))
             (msg nil))
        (cond ((and (< min-pos pos) (< pos max-pos))
               (cider-repl--replace-input (nth pos cider-repl-input-history))
               (setq msg (format "History item: %d" pos)))
              ((not cider-repl-wrap-history)
               (cider-repl--replace-input "") ; :nomis-hack
               (setq msg (cond ((= pos min-pos) "End of history")
                               ((= pos max-pos) "Beginning of history"))))
              (cider-repl-wrap-history
               (setq pos (if (= pos min-pos) max-pos min-pos))
               (setq msg "Wrapped history")))
        (when (or (<= pos min-pos) (<= max-pos pos))
          (when regexp
            (setq msg (concat msg "; no matching item"))))
        (message "%s%s" msg (cond ((not regexp) "")
                                  (t (format "; current regexp: %s" regexp))))
        (setq cider-repl-input-history-position pos)
        (setq this-command 'cider-repl--history-replace)))))

 (t
  (message-box
   "You need to fix `cider-repl--history-replace` for this version of CIDER.")))

;;;; ___________________________________________________________________________

(provide 'nomis-cider-repl-history-hacks)
