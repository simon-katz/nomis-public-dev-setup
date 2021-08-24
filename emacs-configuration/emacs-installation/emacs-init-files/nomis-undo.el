;;; nomis-undo.el --- Some undo stuff -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; ---- nomis/with-atomic-undo ----

(defun nomis/f-with-atomic-undo (f)
  "Helper for `nomis/with-atomic-undo'."
  (let ((handle (prepare-change-group))
        ;; Don't truncate any undo data in the middle of this.
        (undo-outer-limit nil)
        (undo-limit most-positive-fixnum)
        (undo-strong-limit most-positive-fixnum)
        (success nil))
    (unwind-protect
        (progn
          (activate-change-group handle)
          (prog1
              (funcall f)
            (setq success t)))
      (if success
          (progn
            (accept-change-group handle)
            (undo-amalgamate-change-group handle))
        (cancel-change-group handle)))))

(cl-defmacro nomis/with-atomic-undo (&body body)
  "Like `progn' but perform BODY with undo collapsed. A copy of
`atomic-change-group' but with a changes to call
`undo-amalgamate-change-group' and return the result of BODY. See
https://emacs.stackexchange.com/questions/54402/how-to-impliment-a-with-undo-collapse-macro-using-change-group-feature/54411"
  (declare (indent 0))
  `(nomis/f-with-atomic-undo (lambda () ,@body)))

;;;; ___________________________________________________________________________

(provide 'nomis-undo)
;;; nomis-undo.el ends here
