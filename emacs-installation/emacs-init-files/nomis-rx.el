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

(defun nomis/rx/wrap (regexp)
  "Return a non-capturing group that wraps `regexp`."
  (concat "\\(?:"
          regexp
          "\\)"))

(defun nomis/rx/optional (regexp)
  "Return a non-capturing group that matches zero or one occurences
of `regexp`."
  (concat "\\(?:"
          regexp
          "\\)?"))

(defun nomis/rx/or (&rest regexes)
  "Return a non-capturing group that combines `regexes` with an or."
  (concat "\\(?:"
          (apply 'concat
                 (-interpose "\\|"
                             regexes))
          "\\)"))

(defun nomis/rx/one-or-more (regexp)
  "Return a non-capturing group that matches one or more of `regexp`."
  (concat "\\(?:"
          regexp
          "\\)+"))

(defun nomis/rx/zero-or-more (regexp)
  "Return a non-capturing group that matches zero or more of `regexp`."
  (concat "\\(?:"
          regexp
          "\\)*"))

;;;; ___________________________________________________________________________

(provide 'nomis-rx)
