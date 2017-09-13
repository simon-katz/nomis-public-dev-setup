;;;; Tests for nomis-idle-highlight-mode.el

;;;; ___________________________________________________________________________
;;;; ---- Notes ----
;;;;
;;;; - `insert-file-contents-literally` may be useful.

;;;; ___________________________________________________________________________
;;;; ---- Support ----

(defun nomis/funcall-with-temp-buffer (mode-fun string position fun)
  (with-temp-buffer
    (funcall mode-fun)
    (insert string)
    (goto-char position)
    (funcall fun)))

(defun nomis/string->all-buffer-positions (string)
  "All the positions in a buffer whose contents are STRING.
That's a list of successive integers starting at 1 (the first position in the
buffer) and going up to one greater than the length of the string (the last
position in the buffer)."
  (number-sequence 1
                   (1+ (length string))))

(defun nomis/run-fun-in-all-positions-in-temp-buffer (mode-fun string fun)
  (mapcar (lambda (position)
            (nomis/funcall-with-temp-buffer mode-fun
                                            string
                                            position
                                            fun))
          (nomis/string->all-buffer-positions string)))

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

;;;; ___________________________________________________________________________

(defun niht/run-basic-test (mode-fun string)
  (nomis/run-fun-in-all-positions-in-temp-buffer mode-fun
                                                 string
                                                 'nomis-idle-highlight-thing))

;;;; ___________________________________________________________________________
;;;; ---- niht/basic-test ----

(defvar niht/basic-test/string
  "  foo  bar-x
baz-yy   ")

(ert-deftest niht/basic-test ()
  (should (equal (niht/run-basic-test 'emacs-lisp-mode
                                      niht/basic-test/string)
                 (nomis/pairs->list (2 nil)
                                    (4 "foo")
                                    (1 nil)
                                    (6 "bar-x")
                                    (7 "baz-yy")
                                    (3 nil)))))

;;;; ___________________________________________________________________________

(provide 'nomis-idle-highlight-mode-tests)
