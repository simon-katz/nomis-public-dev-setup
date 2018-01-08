;; -*- lexical-binding: t -*-

;;;; Regexp utilities.
;;;; Creates regexps at run time.

;;;; ___________________________________________________________________________

(defun nomis/rx/make-char-match-regexp/broken (str)
  (concat "["
          str
          "]"))

(defun nomis/rx/make-char-mismatch-regexp/broken (str)
  (concat "[^"
          str
          "]"))

;;;; ___________________________________________________________________________

(defun nomis/rx/or (&rest regexes)
  (concat "\\(?:"
          (apply 'concat
                 (-interpose "\\|"
                             regexes))
          "\\)"))

(defun nomis/rx/one-or-more (regexp)
  (concat "\\(?:"
          regexp
          "\\)+"))

;;;; ___________________________________________________________________________

(provide 'nomis-rx)
