;;;; Tests for nomis/idle-highlight-mode.el

;;;; ___________________________________________________________________________
;;;; ---- Notes ----
;;;;
;;;; - `insert-file-contents-literally` may be useful.

;;;; ___________________________________________________________________________
;;;; ---- Support ----

(defun nomis/map-over-positions-in-temp-buffer (set-mode-fun
                                                string
                                                fun
                                                positions)
  "Create a temporary buffer, call SET-MODE-FUN, insert STRING, and return
a list of the results of calling FUN when in the supplied POSITIONS in
the buffer."
  (with-temp-buffer
    (funcall set-mode-fun)
    (insert string)
    (mapcar (lambda (p)
              (goto-char p)
              (funcall fun))
            positions)))

(defun nomis/string->all-buffer-positions (string)
  "A sequence of all the positions in a buffer whose contents are STRING.
That's a sequence of successive integers starting at 1 (the first position in
the buffer) and going up to one greater than the length of the string (the last
position in the buffer)."
  (number-sequence 1
                   (1+ (length string))))

(defun nomis/map-over-temp-buffer (set-mode-fun string fun)
  "Create a temporary buffer, call SET-MODE-FUN, insert STRING, and return
a list of the results of calling FUN in each position in the buffer."
  (let ((positions (nomis/string->all-buffer-positions string)))
    (nomis/map-over-positions-in-temp-buffer set-mode-fun
                                             string
                                             fun
                                             positions)))

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

(defun niht/run-basic-test (set-mode-fun string)
  (nomis/map-over-temp-buffer set-mode-fun
                              string
                              'nomis/idle-highlight-thing))

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
