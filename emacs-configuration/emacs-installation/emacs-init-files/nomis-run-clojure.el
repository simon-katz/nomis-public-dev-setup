;;;; Init stuff -- nomis-run-clojure --  -*- lexical-binding: t -*-

;;## ;;;; ****
;;## ;;;; + Ring bell when you get a Clojure error.
;;## ;;;;   Need to write something a bit different to `nrepl-eval-print'.
;;## ;;;;


(defvar nomis/newline-string "
")

(defun transform-string-value (value)
  (s-replace "\\n"
             nomis/newline-string
             value))

(defun nomis/run-clojure-and-insert-result (clojure-form-as-string
                                            &optional after-fun)
  (cider-interactive-eval clojure-form-as-string
                          (nrepl-make-response-handler
                           (current-buffer)
                           (lambda (buffer value)
                             (prog1
                                 (let ((value (transform-string-value value)))
                                   (with-current-buffer buffer
                                     (insert
                                      (if (derived-mode-p 'cider-clojure-interaction-mode)
                                          (format "\n%s\n" value)
                                        value))))
                               (when after-fun
                                 (funcall after-fun))))
                           (lambda (_buffer out)
                             (cider-emit-interactive-eval-output out))
                           (lambda (_buffer err)
                             (cider-emit-interactive-eval-err-output err))
                           '())))

(defun nomis/run-clojure-no-insert (clojure-form-as-string)
  (cider-interactive-eval clojure-form-as-string
                          (nrepl-make-response-handler
                           (current-buffer)
                           (lambda (buffer value)
                             ;; no-op
                             )
                           (lambda (_buffer out)
                             (cider-emit-interactive-eval-output out))
                           (lambda (_buffer err)
                             (cider-emit-interactive-eval-err-output err))
                           '())))

(provide 'nomis-run-clojure)
