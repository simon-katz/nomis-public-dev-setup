;;;; Init stuff -- nomis-run-clojure --  -*- lexical-binding: t -*-

;;## ;;;; ****
;;## ;;;; + Ring bell when you get a Clojure error.
;;## ;;;;   Need to write something a bit different to `nrepl-eval-print'.
;;## ;;;;

(defun nomis/run-clojure (clojure-form-as-string
                          &optional on-success-fun)
  (cider-interactive-eval clojure-form-as-string
                          (nrepl-make-response-handler
                           (current-buffer)
                           (lambda (buffer value)
                             (when on-success-fun
                               (funcall on-success-fun value)))
                           (lambda (_buffer out)
                             (cider-emit-interactive-eval-output out))
                           (lambda (_buffer err)
                             (cider-emit-interactive-eval-err-output err))
                           '())))

(provide 'nomis-run-clojure)
