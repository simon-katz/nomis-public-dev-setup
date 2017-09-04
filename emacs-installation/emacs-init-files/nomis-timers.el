;;;; Timers

;;;; ___________________________________________________________________________

(defmacro def-nomis/timer-with-fixed-repeats (var
                                              time
                                              repeat
                                              &rest body)
  ;; Allow this file to be reloaded without creating multiple timers.
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

(defmacro def-nomis/timer-with-relative-repeats (var
                                                 time
                                                 repeat
                                                 &rest body)
  ;; Allow this file to be reloaded without creating multiple timers.
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
                              ,@body
                              (,fun-name ,repeat)))))
       (,fun-name ,time))))

;;;; ___________________________________________________________________________

(provide 'nomis-timers)
