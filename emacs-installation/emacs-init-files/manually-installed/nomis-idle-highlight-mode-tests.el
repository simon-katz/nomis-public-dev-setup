;;;; Tests for nomis-idle-highlight-mode.el

;;;; ___________________________________________________________________________
;;;; ---- Notes ----
;;;;
;;;; - (?) Use dash.
;;;; - `insert-file-contents-literally` may be useful.

;;;; ___________________________________________________________________________
;;;; ---- Support ----

(defun nomis/with-temp-buffer-etc (mode-fun string position fun)
  (with-temp-buffer
    (funcall mode-fun)
    (insert string)
    (goto-char position)
    (funcall fun)))

(defun niht/test (mode-fun string position)
  (nomis/with-temp-buffer-etc mode-fun
                              string
                              position
                              'nomis-idle-highlight-thing))

;;;; ___________________________________________________________________________
;;;; ---- niht/basic-test ----

(defvar niht/basic-test/string
  "  foo  bar  baz   ")

(defun niht/run-basic-test (position)
  (niht/test 'emacs-lisp-mode
             niht/basic-test/string
             position))

(ert-deftest niht/basic-test/first-char-whitespace/bug ()
  :expected-result :failed ; The fix is `nomis-looking-at-char-1-whitespace-p`
  (should (equal (niht/run-basic-test 1) nil)))

(ert-deftest niht/basic-test ()
  (should (equal (mapcar 'niht/run-basic-test
                         '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
                 '("  foo" ; bug -- see `niht/basic-test/first-char-whitespace/bug
                   nil
                   "foo" "foo" "foo" "foo"
                   nil
                   "bar" "bar" "bar" "bar"
                   nil
                   "baz" "baz" "baz" "baz"
                   nil nil nil))))

;;;; ___________________________________________________________________________

(provide 'nomis-idle-highlight-mode-tests)
