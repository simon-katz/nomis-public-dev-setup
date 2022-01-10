;;;; nomis-logview.el --- logview tailoring ---  -*- lexical-binding: t -*-

(require 'logview)

;;;; ___________________________________________________________________________
;;;; ---- Globals ----

(setq logview-auto-revert-mode 'auto-revert-tail-mode)

(setq logview-show-ellipses nil)

;;;; ___________________________________________________________________________
;;;; ---- Per-buffer ----

(defun -nomis/logview-setup ()
  ;; Elsewhere (eg in `nomis/org-mode`) we're using `visual-line-mode`, but
  ;; that doesn't work here.  So use `truncate-lines` (which I guess we'd
  ;; decided against before).
  (setq truncate-lines t)
  ;; Avoid `diff-hl` problem of `revert-buffer-preserve-modes` being unbound:
  (diff-hl-mode 0))

(add-hook 'logview-mode-hook
          '-nomis/logview-setup)

;;;; ___________________________________________________________________________

(provide 'nomis-logview)
