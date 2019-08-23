;;;; nomis-repeated-commands.el ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; * nomis/define-repeated-command-stuff

(defvar -nomis/drcs/previous-value
  ;; We only use this when we are repeating, so we can share this across
  ;; all repeatable commands.
  nil)

(cl-defmacro nomis/define-repeated-command-stuff (command-definer-name
                                                  with-stuff-name
                                                  functions-var-name
                                                  next-value)
  `(progn
     (defvar ,functions-var-name '())

     (cl-defmacro ,command-definer-name (name arglist &body body)
       (declare (indent 2))
       `(progn
          (pushnew ',name ,',functions-var-name)
          (defun ,name ,arglist ,@body)))

     (cl-defmacro ,with-stuff-name (initial-value
                                    in-value
                                    &body body)
       (declare (indent 2))
       `(let* ((previous-command-was-one-of-these?
                (member (nomis/org/last-command)
                        ,',functions-var-name))
               (current-place (list (current-buffer)
                                    (point)))
               (%value% (if (and -nomis/drcs/previous-value
                                 previous-command-was-one-of-these?)
                            (let* ((%in-value% ,in-value)
                                   (%previous-value% -nomis/drcs/previous-value))
                              ,',next-value)
                          ,initial-value)))
          (setq -nomis/drcs/previous-value %value%)
          ,@body))))

;;;; ___________________________________________________________________________
;;;; * End

(provide 'nomis-repeated-commands)
