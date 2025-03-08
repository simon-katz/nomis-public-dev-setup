;;; nomis-sexp-navigation.el --- Nomis sexp navigation -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________

(require 'nomis-sexp-utils)

;;;; ___________________________________________________________________________

(defun nomis/backward-top-level-sexp ()
  (interactive)
  (backward-paragraph)
  (when (ignore-errors paredit-mode)
    (while (not (nomis/sexp-at-top-level?))
      (backward-paragraph))))

(defun nomis/forward-top-level-sexp ()
  (interactive)
  (forward-paragraph)
  (when (ignore-errors paredit-mode)
    (while (not (nomis/sexp-at-top-level?))
      (forward-paragraph))))

(progn
  (define-key global-map (kbd "C-z C-<up>")   'nomis/backward-top-level-sexp)
  (define-key global-map (kbd "C-z C-<down>") 'nomis/forward-top-level-sexp))

;;;; ___________________________________________________________________________

(defun nomis/reverse-transpose-sexps (arg)
  (interactive "*p")
  (let* ((start-pos (point)))
    (nomis/with-cleanup-on-non-local-exit
        (transpose-sexps (- arg))
      (goto-char start-pos))))

;;;; † is Option-t on Mac, so "C-†" is physically very like "C-M-t" which does
;;;; `transpose-sexps`.
(define-key global-map (kbd "C-†")   'nomis/reverse-transpose-sexps)
(define-key global-map (kbd "C-M-y") 'nomis/reverse-transpose-sexps)

;;;; For playing:
;;;; (1 2 3 5 4 6 7 8 9)

;;;; ___________________________________________________________________________

(provide 'nomis-sexp-navigation)
