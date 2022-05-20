;;;; Init stuff -- String Utilities.

;;;; ___________________________________________________________________________
;;;; From http://emacswiki.org/emacs/ElispCookbook.

(defun string/ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

;;;; ___________________________________________________________________________

(progn

  (defun string->list-of-chars (s)
    ;; Is there a built-in way to do this?
    ;; Blimey!
    (append s nil))

  (cl-assert (equal (string->list-of-chars "")
                 '()))

  (cl-assert (equal (string->list-of-chars "abc")
                 '(?a ?b ?c))))

;;;; ___________________________________________________________________________

(provide 'nomis-string-utilities)
