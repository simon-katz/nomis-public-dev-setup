;;;; Init stuff -- Indenting s-expressions.

;;;; ___________________________________________________________________________
;;;; ---- Flash forms when indenting ----

(defvar nomis/prog-indent-sexp-flash-duration 0.2)
(defvar nomis/prog-indent-sexp-flash-colour "PeachPuff1")

(defface nomis/prog-indent-sexp-flash-face
  ;; Copied from `eval-sexp-fu-flash`, and changed.
  `((((class color)) ,(list :background nomis/prog-indent-sexp-flash-colour
                            :foreground "black"
                            :bold nil))
    (t (:inverse-video t)))
  "Face for highlighting sexps during indentation."
  :group 'eval-sexp-fu ; is this right?
  )

(defmacro nomis/define-indent-command (command)
  `(progn
     (define-eval-sexp-fu-flash-command ,command
       (eval-sexp-fu-flash (cons (point)
                                 (save-excursion
                                   (forward-sexp)
                                   (point)))))
     (advice-add ',command
                 :around
                 (lambda (orig-fun &rest args)
                   (let* ((eval-sexp-fu-flash-duration
                           nomis/prog-indent-sexp-flash-duration)
                          (eval-sexp-fu-flash-face
                           'nomis/prog-indent-sexp-flash-face))
                     (apply orig-fun args)))
                 '((name . nomis/with-flash-for-indenting)))))

;; The built-in key bindings are:
;; - C-M-q in elisp               indent-pp-sexp
;; - C-M-q in clojure             prog-indent-sexp
;; -   M-q in clojure and elisp   paredit-reindent-defun

(nomis/define-indent-command indent-pp-sexp)
(nomis/define-indent-command prog-indent-sexp)
(nomis/define-indent-command paredit-reindent-defun)

(progn
  ;; Move to start of form when doing `indent-pp-sexp` so that flashing works.
  (advice-add 'indent-pp-sexp
              :around
              (lambda (orig-fun &rest args)
                (save-excursion
                  (nomis-start-of-this-or-enclosing-form)
                  (apply orig-fun args)))
              '((name . nomis/goto-beginning-of-form))))

(progn
  ;; Move to start of form when doing `prog-indent-sexp` so that flashing works.
  (advice-add 'prog-indent-sexp
              :around
              (lambda (orig-fun &rest args)
                (save-excursion
                  (nomis-start-of-this-or-enclosing-form)
                  (apply orig-fun args)))
              '((name . nomis/goto-beginning-of-form))))

(progn
  ;; Want to flash the whole form when doing `paredit-reindent-defun`.
  (advice-add 'paredit-reindent-defun
              :around
              (lambda (orig-fun &rest args)
                (save-excursion
                  (nomis-beginning-of-top-level-form)
                  (let* ((not-in-a-top-level-symbol-p
                          (nomis-looking-at-bracketed-sexp-start)))
                    (if not-in-a-top-level-symbol-p
                        (apply orig-fun args)
                      ;; We are on a top-level symbol. `paredit-reindent-defun`
                      ;; would re-indent a nearby non-symbol top-level form, so
                      ;; don't do anything.
                      (nomis/beep)))))
              '((name . nomis/goto-beginning-of-this-defun))))

;;;; ___________________________________________________________________________

(provide 'nomis-indent-sexp)
