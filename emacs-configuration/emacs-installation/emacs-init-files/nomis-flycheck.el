;;; Init stuff -- flycheck mode

;;; Code:

;;;; Require things

(require 'flycheck)
(require 'flycheck-projectile)
(require 'projectile)

;;;; A heading

(pushnew 'idle-buffer-switch flycheck-check-syntax-automatically)

(setq flycheck-error-list-format
      `[("File" 12)
        ("Line" 5 flycheck-error-list-entry-< :right-align t)
        ("Col" 3 nil :right-align t)
        ("Level" 8 flycheck-error-list-entry-level-<)
        ("ID" 18 t)
        (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)])

;;;; A heading

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
                                             `(foreground-color
                                               .
                                               ,(if (nomis/dark-background-mode?)
                                                    "orange"
                                                  "blue"))
                                             error-checker-info)
                          (concat error-checker-info
                                  " "
                                  raw-value))
                      raw-value)))
                '((name . nomis/add-checker-name-to-flycheck-message*))))
   (t
    ;; We are doing this generically in `mp-flycheck-eldoc` now.
    )))

;;;; A heading

(setopt flycheck-emacs-lisp-load-path 'inherit)

(setopt flycheck-temp-prefix "_no-commit_flycheck_")

;;;; A heading

;; Provide some easier-to-type key bindings than the built-in ones

(let* ((m flycheck-mode-map))
  (define-key m (kbd "C-z C-<left>")  'flycheck-previous-error)
  (define-key m (kbd "C-z C-<right>") 'flycheck-next-error))

;;;; Per-project `flycheck-projectile-list-errors` buffers

(defun nomis/flycheck-projectile-list-errors-per-project (orig-fn &optional dir)
  "Call ORIG-FN with `flycheck-projectile-error-list-buffer' set
per-project, so that multiple project error buffers can coexist.
See also `nomis/flycheck-projectile-auto-switch'."
  (require 'projectile)
  (setq flycheck-projectile-error-list-buffer
        (format "*Project errors: %s*" (projectile-project-name)))
  (funcall orig-fn dir))

(advice-add 'flycheck-projectile-list-errors
            :around #'nomis/flycheck-projectile-list-errors-per-project)

(defun nomis/flycheck-projectile-auto-switch (_window_or_frame)
  "Transfer `flycheck-projectile' live updates on project switch.
Called via `window-buffer-change-functions' and
`window-selection-change-functions'.  Does nothing if the
per-project error buffer for the current buffer's project does
not exist yet."
  (let* ((buf (current-buffer)))
    (when (buffer-file-name buf)
      (with-current-buffer buf
        (when (and (fboundp 'projectile-project-p)
                   (projectile-project-p))
          (let* ((project-name (projectile-project-name))
                 (buf-name     (format "*Project errors: %s*" project-name)))
            (when (and (get-buffer buf-name)
                       (not (equal project-name
                                   flycheck-projectile--project)))
              (save-window-excursion
                (flycheck-projectile-list-errors)))))))))

(add-hook 'window-buffer-change-functions
          #'nomis/flycheck-projectile-auto-switch)

(add-hook 'window-selection-change-functions
          #'nomis/flycheck-projectile-auto-switch)

;;; End

(provide 'nomis-flycheck)
