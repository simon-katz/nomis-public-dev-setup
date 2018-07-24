;;;; Init stuff -- Indenting s-expressions.

;;;; ___________________________________________________________________________

(require 'eval-sexp-fu)
(require 'nomis-sexp-utils)

;;;; ___________________________________________________________________________
;;;; ---- Support for flashing forms when indenting ----

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

(defmacro nomis/define-indent-command (indentation-command
                                       &rest move-and-call-advice-commands)
  "For `indentation-command`, add advice to do the following:
   - Position the cursor at the start of the targetted form.
   - Flash the targetted form.
   Within `move-and-call-advice-commands`, you should move the cursor to the
   start of the targetted form and call `(%do-indentation%)` to run the
   underlying command."
  (declare (indent 1))
  `(progn
     (define-eval-sexp-fu-flash-command ,indentation-command
       (eval-sexp-fu-flash (cons (point)
                                 (save-excursion
                                   (forward-sexp)
                                   (point)))))
     (advice-add ',indentation-command
                 :around
                 ,(case 2 ; an easy way to turn off the advice if you feel a need to
                    (1 `(lambda (orig-fun &rest args)
                          (apply orig-fun args)))
                    (2 `(lambda (orig-fun &rest args)
                          (let* ((eval-sexp-fu-flash-duration
                                  nomis/prog-indent-sexp-flash-duration)
                                 (eval-sexp-fu-flash-face
                                  'nomis/prog-indent-sexp-flash-face))
                            (cl-labels ((%do-indentation%
                                         ()
                                         (apply orig-fun args)))
                              (save-excursion
                                ,@move-and-call-advice-commands))))))
                 '((name . nomis/with-flash-for-indenting)))))

;;;; ___________________________________________________________________________
;;;; ---- Flash forms when indenting ----

;;;; The built-in key bindings are:
;;;; - C-M-q in elisp               indent-pp-sexp
;;;; - C-M-q in clojure             prog-indent-sexp
;;;; -   M-q in clojure and elisp   paredit-reindent-defun
;;;;

(nomis/define-indent-command indent-pp-sexp
  (nomis/start-of-this-or-enclosing-form)
  (%do-indentation%))

(nomis/define-indent-command prog-indent-sexp
  (nomis/start-of-this-or-enclosing-form)
  (%do-indentation%))

(nomis/define-indent-command paredit-reindent-defun
  (nomis/beginning-of-top-level-form)
  (let* ((in-a-top-level-symbol-p (not
                                   (nomis/looking-at-bracketed-sexp-start))))
    (cond (in-a-top-level-symbol-p
           ;; We are on a top-level symbol. `paredit-reindent-defun` would
           ;; re-indent a nearby non-symbol top-level form; instead of that
           ;; don't do anything.
           (nomis/beep))
          ((or (bound-and-true-p cider-mode)
               (bound-and-true-p cider-repl-mode))
           ;; `paredit-reindent-defun` in Clojure doesn't indent properly.
           ;; I think it does the cljfmt thing rather than the CIDER thing.
           ;; Instead, do something that gives proper indentation.
           (prog-indent-sexp))
          (t
           (%do-indentation%)))))

;;;; ___________________________________________________________________________

(provide 'nomis-indent-sexp)
