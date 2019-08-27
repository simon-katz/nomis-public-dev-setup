;;;; nomis-repeated-commands.el ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; * nomis/define-repeated-command-stuff

(defun -nomis/drcs/bring-within-range (v maximum)
  (when (or (< v 0)
            (> v maximum))
    (nomis/grab-user-attention/low))
  (min (max 0 v)
       maximum))

(defun -nomis/drcs/do-the-biz (name
                               maximum
                               value
                               new-value-action-fun)
  (let* ((new-value (-> value
                        (-nomis/drcs/bring-within-range maximum))))
    (prog1
        (funcall new-value-action-fun new-value)
      (let* ((message-fun ))
        (funcall (if (not (featurep 'nomis-popup))
                     #'message
                   #'nomis/popup/message)
                 (or *-norg/level-format-string* ; TODO
                     "[%s / %s]")
                 new-value
                 maximum)))))

(cl-defmacro nomis/define-repeated-command-stuff (name
                                                  with-stuff-name/set
                                                  maximum-fun
                                                  new-value-action-fun)
  (declare (indent 1))
  `(progn

     (defvar ,name nil) ; so that definition can be found -- and must provide a value for that to work!

     (defun ,with-stuff-name/set (value)
       (let* ((maximum (funcall ,maximum-fun)))
         (-nomis/drcs/do-the-biz ',name
                                 maximum
                                 value
                                 ,new-value-action-fun)))))

;;;; ___________________________________________________________________________
;;;; * End

(provide 'nomis-repeated-commands)
