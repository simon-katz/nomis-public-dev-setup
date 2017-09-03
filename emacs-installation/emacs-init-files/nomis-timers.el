;;;; Timers

;;;; ___________________________________________________________________________

(defmacro def-nomis/timer (var
                           time
                           repeat
                           &rest body)
  ;; Allow this file to be reloaded without creating multiple timers.
  `(progn
     (when (and (boundp ',var)
                (not (null ,var)))
       (assert (timerp ,var))
       (cancel-timer ,var))
     (setq ,var
           (run-at-time ,time
                        ,repeat
                        (lambda ()
                          ,@body)))))

;;;; ___________________________________________________________________________

(provide 'nomis-timers)
