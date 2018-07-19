;;;; Timers

;;;; ___________________________________________________________________________

(defmacro nomis/def-timer-with-fixed-repeats (var
                                              time
                                              repeat
                                              &rest body)
  ;; Allow this file to be reloaded without creating multiple timers.
  (declare (indent 3))
  `(progn
     (defvar ,var)
     (when (and (boundp ',var)
                (not (null ,var)))
       (ignore-errors
         (cancel-timer ,var)))
     (setq ,var
           (run-at-time ,time
                        ,repeat
                        (lambda ()
                          ,@body)))))

(defmacro nomis/def-timer-with-relative-repeats (var
                                                 time
                                                 &rest body)
  ;; Allow this file to be reloaded without creating multiple timers.
  (declare (indent 2))
  (let ((fun-name (intern (concat "____unlikely-prefix/nomis-timer/"
                                  (symbol-name var)))))
    `(progn
       (defvar ,var)
       (when (and (boundp ',var)
                  (not (null ,var)))
         (ignore-errors
           (cancel-timer ,var)))
       (defun ,fun-name (next-time)
         (setq ,var
               (run-at-time next-time
                            nil
                            (lambda ()
                              (let ((res (progn ,@body)))
                                (when (and (listp res)
                                           (eql (first res) :repeat)
                                           (numberp (second res)))
                                  (,fun-name (second res))))))))
       (,fun-name ,time))))

;;;; ___________________________________________________________________________

(provide 'nomis-timers)
