;;;; Tests for nomis-idle-highlight-mode.el

;;;; ___________________________________________________________________________
;;;; ---- Notes ----
;;;;
;;;; - `insert-file-contents-literally` may be useful.

;;;; ___________________________________________________________________________
;;;; ---- Support ----

(defun string->position-sequence (string)
  "All the positions in a buffer whose contents are STRING.
That's a list of successive integers starting at 1 (the first position in the
buffer) and going up to one greater than the length of the string (the last
position in the buffer)."
  (number-sequence 1
                   (1+ (length string))))

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

(ert-deftest niht/basic-test ()
  (should (equal (mapcar 'niht/run-basic-test
                         (string->position-sequence niht/basic-test/string))
                 '(nil nil
                   "foo" "foo" "foo" "foo"
                   nil
                   "bar" "bar" "bar" "bar"
                   nil
                   "baz" "baz" "baz" "baz"
                   nil nil nil))))

;;;; ___________________________________________________________________________

(provide 'nomis-idle-highlight-mode-tests)
