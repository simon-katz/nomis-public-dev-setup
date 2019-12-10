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
              "32snapshot (package: 20191126.2142)"))
    (defvar *-nomis/add-checker-name-to-flycheck-message?* nil)
    (advice-add 'flycheck-display-error-messages
                :around
                (lambda (orig-fun &rest args)
                  (let* ((*-nomis/add-checker-name-to-flycheck-message?* t))
                    (apply orig-fun args)))
                '((name . nomis/add-checker-name-to-flycheck-message*)))
    (advice-add 'flycheck-error-format-message-and-id
                :around
                (lambda (orig-fun err)
                  (let* ((raw-value (funcall orig-fun err)))
                    (if *-nomis/add-checker-name-to-flycheck-message?*
                        (let* ((error-checker-info
                                (concat "("
                                        (symbol-name (flycheck-error-checker err))
                                        ")")))
                          (put-text-property 0
                                             (length error-checker-info)
                                             'face
                                             '(foreground-color . "blue")
                                             error-checker-info)
                          (concat error-checker-info
                                  " "
                                  raw-value))
                      raw-value)))
                '((name . nomis/add-checker-name-to-flycheck-message*))))
   (t
    (message-box
     "You need to fix our 'include checker name in flycheck messages' for this version of flycheck."))))

;;;; ___________________________________________________________________________

(provide 'nomis-flycheck)
