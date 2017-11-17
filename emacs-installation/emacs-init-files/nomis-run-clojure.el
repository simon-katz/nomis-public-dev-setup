;;;; Init stuff -- nomis-run-clojure

;;## ;;;; ****
;;## ;;;; + Ring bell when you get a Clojure error.
;;## ;;;;   Need to write something a bit different to `nrepl-eval-print'.
;;## ;;;;


(defvar nomis-newline-string "
")

(defun transform-string-value (value)
  (s-replace "\\n"
             nomis-newline-string
             value))

(defun nomis/run-clojure (clojure-form-as-string)
  (cider-interactive-eval clojure-form-as-string
                          (nrepl-make-response-handler
                           (current-buffer)
                           (lambda (buffer value)
                             (let ((value (transform-string-value value)))
                               (with-current-buffer buffer
                                 (insert
                                  (if (derived-mode-p 'cider-clojure-interaction-mode)
                                      (format "\n%s\n" value)
                                    value)))))
                           (lambda (_buffer out)
                             (cider-emit-interactive-eval-output out))
                           (lambda (_buffer err)
                             (cider-emit-interactive-eval-err-output err))
                           '())))

(provide 'nomis-run-clojure)
