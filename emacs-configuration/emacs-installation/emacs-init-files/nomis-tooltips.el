;;; nomis-tooltips.el ---tooltips setup -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________

(setq tooltip-hide-delay (* 10 365 24 60 60)) ; should be enough

;;;; ___________________________________________________________________________

;; Some hacky stuff for making tooltip messages appear in the *Messages* buffer:

;; ;; Reset (assuming `tooltip-mode` is on):
;; (setq show-help-function 'tooltip-show-help); the default (when using tooltips)

;; ;; Send to *Messages* buffer:

;; (defun -nomis/show-help-in-messages-buffer-too (msg)
;;   (let* ((inhibit-message t)) (message "%s" msg))
;;   (tooltip-show-help msg))

;; (setq show-help-function '-nomis/show-help-in-messages-buffer-too)

;;;; ___________________________________________________________________________

(provide 'nomis-tooltips)
