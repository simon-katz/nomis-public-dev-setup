;;;; Init stuff -- flycheck mode

;;;; ___________________________________________________________________________

(require 'flycheck)

;;;; ___________________________________________________________________________

(pushnew 'idle-buffer-switch flycheck-check-syntax-automatically)

;;;; ___________________________________________________________________________

(progn ; Include checker name in flycheck messages in echo area.
  (cond
   ((member (flycheck-version)
            '("31"
              "32snapshot (package: 20191126.2142)"
              "32snapshot (package: 20210708.1337)"
              "32snapshot (package: 20210825.1804)"
              "33snapshot (package: 20230306.414)"))
    (defvar *-nomis/add-checker-name-to-flycheck-message?* nil)
    (advice-add 'flycheck-display-error-messages
                :around
                (lambda (orig-fun &rest args)
                  (let* ((*-nomis/add-checker-name-to-flycheck-message?* t))
                    (apply orig-fun args)))
                '((name . nomis/add-checker-name-to-flycheck-message*)))
    (advice-add 'flycheck-error-format-message-and-id
                :around
                (lambda (orig-fun err &optional include-snippet)
                  (let* ((raw-value (funcall orig-fun err)))
                    (if *-nomis/add-checker-name-to-flycheck-message?*
                        (let* ((error-checker-info
                                (concat "["
                                        (symbol-name (flycheck-error-checker err))
                                        "]")))
                          (put-text-property 0
                                             (length error-checker-info)
                                             'face
                                             `(foreground-color . ,(case 2
                                                                     (1 "blue")
                                                                     (2 "yellow")))
                                             error-checker-info)
                          (concat error-checker-info
                                  " "
                                  raw-value))
                      raw-value)))
                '((name . nomis/add-checker-name-to-flycheck-message*))))
   (t
    (message-box
     "You need to fix `nomis/add-checker-name-to-flycheck-message*` for this version of `flycheck`."))))

;;;; ___________________________________________________________________________
;;;; Provide some easier-to-type key bindings than the built-in ones

(let* ((m flycheck-mode-map))
  (define-key m (kbd "C-z C-<left>")  'flycheck-previous-error)
  (define-key m (kbd "C-z C-<right>") 'flycheck-next-error))

;;;; ___________________________________________________________________________

(provide 'nomis-flycheck)
