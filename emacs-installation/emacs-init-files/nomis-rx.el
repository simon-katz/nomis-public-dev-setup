;; -*- lexical-binding: t -*-

;;;; Regexp utilities.
;;;; Creates regexps at run time.

;;;; ___________________________________________________________________________

(defun nomis/rx/or (&rest regexes)
  (concat "\\(?:"
          (apply 'concat
                 (-interpose "\\|"
                             regexes))
          "\\)"))

;;;; ___________________________________________________________________________

(provide 'nomis-rx)
