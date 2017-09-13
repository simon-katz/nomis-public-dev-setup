;;;; Tests for nomis-idle-highlight-mode.el

;;;; ___________________________________________________________________________
;;;; ---- Notes ----
;;;;
;;;; - `insert-file-contents-literally` may be useful.

;;;; ___________________________________________________________________________
;;;; ---- Support ----

(defun nomis/string->all-positions (string)
  "All the positions in a buffer whose contents are STRING.
That's a list of successive integers starting at 1 (the first position in the
buffer) and going up to one greater than the length of the string (the last
position in the buffer)."
  (number-sequence 1
                   (1+ (length string))))

(defmacro nomis/pairs->list (&rest pairs)
  "Create a list as specified by PAIRS.
Let the pairs be (N-1 Obj-1), (N-2 Obj-2), etc.
Each Ni is a non-negative integer.
Each Obj-i is any object.
The result is the concatenation of:
     N-1 copies of Obj-1
     N-2 copies of Obj-2
     etc."
  `(funcall 'concatenate
            'list
            ,@(mapcar (lambda (pair)
                        `(make-list ,(first pair)
                                    ,(second pair)))
                      pairs)))

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
                         (nomis/string->all-positions niht/basic-test/string))
                 (nomis/pairs->list (2 nil)
                                    (4 "foo")
                                    (1 nil)
                                    (4 "bar")
                                    (1 nil)
                                    (4 "baz")
                                    (3 nil)))))

;;;; ___________________________________________________________________________

(provide 'nomis-idle-highlight-mode-tests)
