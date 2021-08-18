;;; nomis-sexp-navigation.el --- Nomis sexp navigation -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________

(require 'nomis-sexp-utils)

;;;; ___________________________________________________________________________

(defun nomis/backward-top-level-sexp ()
  (interactive)
  (backward-paragraph)
  (when (ignore-errors paredit-mode)
    (while (not (nomis/at-top-level?))
      (backward-paragraph))))

(defun nomis/forward-top-level-sexp ()
  (interactive)
  (forward-paragraph)
  (when (ignore-errors paredit-mode)
    (while (not (nomis/at-top-level?))
      (forward-paragraph))))

(progn
  (define-key global-map (kbd "C-z C-<up>")   'nomis/backward-top-level-sexp)
  (define-key global-map (kbd "C-z C-<down>") 'nomis/forward-top-level-sexp))

;;;; ___________________________________________________________________________

(provide 'nomis-sexp-navigation)
