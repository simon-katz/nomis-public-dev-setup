;;;; Init stuff -- Slime eval.

;;;; ___________________________________________________________________________
;;;; ---- Functions to help nicify evaluation commands ----

(defun issue-evaluating-message ()
  (message "Evaluating..."))

(defun move-to-end-of-message-buffer ()
  ;; This works mostly, but not always.
  ;; Sometimes (not always) gets mucked up when there's lots of
  ;; output, or maybe by long output lines.
  ;; When mucked up, repeatedly evaluating something that gives short
  ;; output will eventually cause things to fix themselves.
  (let* ((message-buffer (get-buffer "*Messages*"))
         (message-window (get-buffer-window message-buffer t)))
    (when message-window
      (set-window-point message-window
                        (+ 1 ; without this, sometimes first line of
                             ; output is below last line of window 
                           (buffer-size message-buffer)))
      ;; (set-window-vscroll message-window
      ;;                     (+ 1
      ;;                        (window-vscroll message-window)))
      )))

;;;; ___________________________________________________________________________
;;;; ---- Advice to print "Evaluating..." message when evaluating ----

;; FIXME: Does Emacs have macrolet or similar to allow simplification
;; of all this?
;; FIXME: Is there some lower-level function that all these call, that
;; you could advise instead?

(defadvice eval-defun (before print-evaluation-message ())
  (issue-evaluating-message))

(defadvice eval-last-sexp (before print-evaluation-message ())
  (issue-evaluating-message))

(defadvice slime-eval-defun (before print-evaluation-message ())
  (issue-evaluating-message))

(defadvice slime-eval-last-expression (before print-evaluation-message ())
  (issue-evaluating-message))

;;;; ___________________________________________________________________________
;;;; ---- Advice to move to end of *Messages* buffer after evaluating ----

(defadvice eval-defun (after move-to-end-of-message-buffer ())
  (move-to-end-of-message-buffer))

(defadvice eval-last-sexp (after move-to-end-of-message-buffer ())
  (move-to-end-of-message-buffer))

(defadvice slime-eval-defun (after move-to-end-of-message-buffer ())
  (move-to-end-of-message-buffer))

(defadvice slime-eval-last-expression (after move-to-end-of-message-buffer ())
  (move-to-end-of-message-buffer))

;;;; ___________________________________________________________________________
;;;; ---- Activate advice ----

(ad-activate 'eval-defun)
(ad-activate 'eval-last-sexp)
(ad-activate 'slime-eval-defun)
(ad-activate 'slime-eval-last-expression)


;;;; ___________________________________________________________________________

(provide 'nomis-slime-eval)
