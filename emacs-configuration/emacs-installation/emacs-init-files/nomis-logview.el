;;;; nomis-logview.el --- logview tailoring ---  -*- lexical-binding: t -*-

(require 'logview)

;;;; ___________________________________________________________________________
;;;; ---- Globals ----

(setq logview-auto-revert-mode 'auto-revert-tail-mode)

(setq logview-show-ellipses nil)

(pushnew '("nomis-extra: ISO 8601 datetime + millis using comma"
           (java-pattern . "yyyy-MM-dd HH:mm:ss,SSS"))
         logview-additional-timestamp-formats
         :test 'equal)

;;;; ___________________________________________________________________________
;;;; ---- Per-buffer ----

(defun -nomis/logview-setup ()
  ;; Elsewhere (eg in `nomis/org-mode`) we're using `visual-line-mode`, but
  ;; that doesn't work here.  So use `truncate-lines` (which I guess we'd
  ;; decided against before).
  (setq truncate-lines t))

(add-hook 'logview-mode-hook
          '-nomis/logview-setup)

;;;; ___________________________________________________________________________

(provide 'nomis-logview)
