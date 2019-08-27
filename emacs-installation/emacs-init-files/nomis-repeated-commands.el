;;;; nomis-repeated-commands.el ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; ____ * Require things

(require 'popup nil t)

;;;; ___________________________________________________________________________
;;;; * nomis/define-repeated-command-stuff

(defun -nomis/drcs/bring-within-range (v maximum)
  (when (or (< v 0)
            (> v maximum))
    (nomis/grab-user-attention/low))
  (min (max 0 v)
       maximum))

(defvar -nomis/drcs/most-recent-popup nil)

(defun nomis/popup/message (format-string &rest args)
  (let* ((msg (apply #'format format-string args)))
    (run-at-time 0
                 nil
                 (lambda ()
                   (when (and -nomis/drcs/most-recent-popup
                              (popup-live-p -nomis/drcs/most-recent-popup))
                     (popup-delete -nomis/drcs/most-recent-popup)
                     (setq -nomis/drcs/most-recent-popup nil))
                   (let* ((popup
                           (popup-tip msg
                                      :nowait t
                                      :point (save-excursion
                                               (unless (get-char-property
                                                        (point)
                                                        'invisible)
                                                 (ignore-errors
                                                   (previous-line)))
                                               (point)))))
                     (setq -nomis/drcs/most-recent-popup popup)
                     (run-at-time 1
                                  nil
                                  (lambda ()
                                    (when (popup-live-p popup)
                                      (popup-delete popup)))))))))

(defun -nomis/drcs/do-the-biz (name
                               maximum
                               value
                               new-value-action-fun)
  (let* ((new-value (-> value
                        (-nomis/drcs/bring-within-range maximum))))
    (prog1
        (funcall new-value-action-fun new-value)
      (let* ((message-fun ))
        (funcall (if (not (featurep 'popup))
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
