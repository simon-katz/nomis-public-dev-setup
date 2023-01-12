;; -*- lexical-binding: t -*-

;;;; Regexp utilities.
;;;; Creates regexps at run time.

;;;; ___________________________________________________________________________

;;;; I looked into fixing these. 2019-07-28
;;;;
;;;; A few points:
;;;;
;;;; They're misnamed -- the args are things that go between [ and ] in
;;;; a regexp.
;;;;
;;;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Special.html
;;;; - It says "The usual regexp special characters are not special inside a
;;;;   character alternative. A completely different set of characters is
;;;;   special inside character alternatives: ‘]’, ‘-’ and ‘^’.".
;;;;
;;;; You had some assertions around:
;;;; - 93 ; ?]  (an uncommented ?] caused bracket matching problems!)
;;;; - ?-
;;;; - ?^
;;;; - `(aref str 0)`
;;;; - `(rest (string->list-of-chars str))`
;;;;
;;;; It's not easy to check the arguments for being valid to go between [ and ]
;;;; because you can have eg [:alnum:] as part of the arg.
;;;;
;;;; So for now I've given up.

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

(defun nomis/rx/opt (regexp)
  "Return a non-capturing group that matches zero or more of `regexp`."
  (concat "\\(?:"
          regexp
          "\\)?"))

;;;; ___________________________________________________________________________

(provide 'nomis-rx)
