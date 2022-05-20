;;;; ___________________________________________________________________________
;;;; From nomis-dirtee

(defun zzzz-nomis/dirtree/remove-trailing-slash (s)
  (replace-regexp-in-string "/\\'" "" s))

(defun zzzz-nomis/dirtree/full-filename->list-from-file-system-root (full-filename)
  (cl-assert (eq ?/ (string-to-char full-filename)))
  (->> (substring full-filename 1)
       zzzz-nomis/dirtree/remove-trailing-slash
       (s-split "/")))

(progn ; tests
  (let ((strings '("/aa/bbb/cccc/"
                   "/aa/bbb/cccc")))
    (cl-assert (equal (-map #'zzzz-nomis/dirtree/remove-trailing-slash
                         strings)
                   '("/aa/bbb/cccc"
                     "/aa/bbb/cccc")))
    (cl-assert (equal (-map #'zzzz-nomis/dirtree/full-filename->list-from-file-system-root
                         strings)
                   '(("aa" "bbb" "cccc")
                     ("aa" "bbb" "cccc"))))))
