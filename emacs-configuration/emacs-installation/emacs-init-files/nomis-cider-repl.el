;;;; Init stuff -- CIDER REPL --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(cond
 ((member (pkg-info-package-version 'cider)
          '((20220830 500)))

  (defconst nomis/cider-repl/kaocha-autotest-regexp
    "^nomis-auto: *[0-9]*%")

  (defvar nomis/cider-repl/kaocha-autotest-start-pos
    nil)

  (advice-add
   'cider-repl--emit-output
   :before
   (lambda (_buffer string _face)
     (cl-flet ((delete-back-to-autotest-start
                ()
                (let* ((inhibit-read-only t))
                  (delete-region nomis/cider-repl/kaocha-autotest-start-pos
                                 (point-max)))))
       (if (string-match-p nomis/cider-repl/kaocha-autotest-regexp
                           string)
           (if nomis/cider-repl/kaocha-autotest-start-pos
               (delete-back-to-autotest-start)
             (setq nomis/cider-repl/kaocha-autotest-start-pos (point-max)))
         (setq nomis/cider-repl/kaocha-autotest-start-pos nil))))
   '((name . nomis/-delete-duplicate-kaocha-autotest-lines))))

 (t
  (message-box
   "You need to fix `nomis/-delete-duplicate-kaocha-autotest-lines`` for this version of vterm.")))

;; (advice-remove 'cider-repl--emit-output 'nomis/-delete-duplicate-kaocha-autotest-lines)

;;;; ___________________________________________________________________________

(provide 'nomis-cider-repl)
